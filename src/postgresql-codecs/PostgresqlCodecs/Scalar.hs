{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Scalar where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import PostgresqlCodecs.Core (Report (..), Resolver, Scalar (..), TypeOids (..))
import PostgresqlCodecs.Prelude
import qualified PostgresqlTypes.Algebra as PostgresqlTypes
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | OID of the scalar's own element type, resolved by name when not statically known.
-- Returns 0 when the resolver cannot determine it.
--
-- This is what goes into a binary array header — always the base OID, regardless of
-- dimensionality. Contrast with 'declaredOid'.
elementOid :: Scalar a -> Resolver -> Word32
elementOid scalar resolver =
  case scalar.baseOid of
    Just o -> o
    Nothing -> (resolver scalar.schemaName scalar.typeName).baseOid

-- | OID of the type occupying a parameter, result-column or composite-field slot at the given
-- dimensionality: the array type when @dims > 0@, the base type otherwise. Resolved by name when not
-- statically known. Returns 0 when the resolver cannot determine it.
declaredOid :: Scalar a -> Int -> Resolver -> Word32
declaredOid scalar dims resolver =
  if dims > 0
    then case scalar.arrayOid of
      Just o -> o
      Nothing -> (resolver scalar.schemaName scalar.typeName).arrayOid
    else elementOid scalar resolver

-- | Derive a 'Scalar' from a @postgresql-types@ 'PostgresqlTypes.IsBinaryPrimitive' instance,
-- supporting both the binary and textual wire formats.
binaryPrimitive :: forall a. (PostgresqlTypes.IsBinaryPrimitive a) => Scalar a
binaryPrimitive =
  let -- Convert decoding errors between layers (they have identical shapes)
      convertError :: PostgresqlTypes.DecodingError -> Report
      convertError PostgresqlTypes.DecodingError {location, reason} =
        case reason of
          PostgresqlTypes.ParsingDecodingErrorReason details input ->
            Report
              { message = "Parsing error",
                suggestion = "",
                details =
                  [ ("details", details),
                    ("inputLocation", "[" <> Text.intercalate ", " location <> "]"),
                    ("inputOffset", "0"),
                    ("failingInput", Text.Encoding.decodeUtf8With (\_ _ -> Just '?') input)
                  ]
              }
          PostgresqlTypes.UnexpectedValueDecodingErrorReason expected actual ->
            Report
              { message = "Unexpected value",
                suggestion = "",
                details =
                  [ ("details", "Expected: " <> expected <> ", got: " <> actual),
                    ("inputLocation", "[" <> Text.intercalate ", " location <> "]"),
                    ("inputOffset", "0"),
                    ("failingInput", actual),
                    ("expectedInputs", "[" <> expected <> "]")
                  ]
              }
          PostgresqlTypes.UnsupportedValueDecodingErrorReason details value ->
            Report
              { message = "Unsupported value",
                suggestion = "",
                details =
                  [ ("details", details),
                    ("inputLocation", "[" <> Text.intercalate ", " location <> "]"),
                    ("inputOffset", "0"),
                    ("failingInput", value)
                  ]
              }
      resolvedSchemaName = fromMaybe "" (untag (PostgresqlTypes.schemaName @a))
   in Scalar
        { schemaName = resolvedSchemaName,
          typeName = untag (PostgresqlTypes.typeName @a),
          baseOid = untag (PostgresqlTypes.baseOid @a),
          arrayOid = untag (PostgresqlTypes.arrayOid @a),
          supportsBinary = True,
          -- Not every 'PostgresqlTypes.IsPrimitive' instance knows its OIDs statically
          -- (@citext@ and @hstore@ know neither), and both are needed: the base OID by
          -- 'elementOid' and the array OID by 'declaredOid'. Announce the type as
          -- unresolved whenever either is missing.
          unresolvedTypes =
            if isNothing (untag (PostgresqlTypes.baseOid @a)) || isNothing (untag (PostgresqlTypes.arrayOid @a))
              then Set.singleton (resolvedSchemaName, untag (PostgresqlTypes.typeName @a))
              else mempty,
          binaryEncoder = \_resolver a -> Right (PostgresqlTypes.binaryEncoder a),
          binaryDecoder = \_resolver -> fmap (first convertError) (PostgresqlTypes.binaryDecoder @a),
          textualEncoder = \_resolver a -> Right (PostgresqlTypes.textualEncoder a),
          textualDecoder = \_resolver -> PostgresqlTypes.textualDecoder
        }

-- | Derive a 'Scalar' from a @postgresql-types@ 'PostgresqlTypes.IsPrimitive' instance that has
-- no 'PostgresqlTypes.IsBinaryPrimitive' instance, supporting only the textual wire format.
textualPrimitive :: forall a. (PostgresqlTypes.IsPrimitive a) => Scalar a
textualPrimitive =
  let unsupported =
        Report
          { message = "Binary format not supported",
            suggestion = "Use the textual wire format for this type",
            details = [("type", untag (PostgresqlTypes.typeName @a))]
          }
      resolvedSchemaName = fromMaybe "" (untag (PostgresqlTypes.schemaName @a))
   in Scalar
        { schemaName = resolvedSchemaName,
          typeName = untag (PostgresqlTypes.typeName @a),
          baseOid = untag (PostgresqlTypes.baseOid @a),
          arrayOid = untag (PostgresqlTypes.arrayOid @a),
          supportsBinary = False,
          unresolvedTypes =
            if isNothing (untag (PostgresqlTypes.baseOid @a)) || isNothing (untag (PostgresqlTypes.arrayOid @a))
              then Set.singleton (resolvedSchemaName, untag (PostgresqlTypes.typeName @a))
              else mempty,
          binaryEncoder = \_resolver _a -> Left unsupported,
          binaryDecoder = \_resolver -> pure (Left unsupported),
          textualEncoder = \_resolver a -> Right (PostgresqlTypes.textualEncoder a),
          textualDecoder = \_resolver -> PostgresqlTypes.textualDecoder
        }

-- | Build a 'Scalar' for a PostgreSQL enum type from a label↔value association list.
-- The schema name, type name, and label\/value pairs must match the server-side definition.
enum :: (Ord a) => Text -> Text -> [(Text, a)] -> Scalar a
enum schema tName variants =
  let byValue = Map.fromList (map (\(l, v) -> (v, l)) variants)

      -- Binary encoder: encode as text (enums are text in PostgreSQL wire protocol)
      encoder val =
        case Map.lookup val byValue of
          Just label -> Right (Write.textUtf8 label)
          Nothing ->
            Left
              Report
                { message = "Unsupported enum value",
                  suggestion = "",
                  details = [("schema", schema), ("type", tName)]
                }

      -- Binary decoder: decode from text and look up in variants map
      decoder = do
        bytes <- PtrPeeker.remainderAsByteString
        case Text.Encoding.decodeUtf8' bytes of
          Left e ->
            pure
              ( Left
                  Report
                    { message = "Invalid UTF-8",
                      suggestion = "",
                      details =
                        [ ("details", fromString (show e)),
                          ("inputLocation", "[]"),
                          ("inputOffset", "0"),
                          ("failingInput", Text.Encoding.decodeUtf8With (\_ _ -> Just '?') bytes)
                        ]
                    }
              )
          Right label ->
            case lookup label variants of
              Just val -> pure (Right val)
              Nothing ->
                pure
                  ( Left
                      Report
                        { message = "Unexpected enum value",
                          suggestion = "",
                          details =
                            [ ("details", "Expected one of: " <> Text.intercalate ", " (map fst variants)),
                              ("inputLocation", "[]"),
                              ("inputOffset", "0"),
                              ("failingInput", label),
                              ("expectedInputs", "[" <> Text.intercalate ", " (map fst variants) <> "]")
                            ]
                        }
                  )

      -- Textual encoder: just output the label
      textEncoder val =
        case lookup val (map (\(l, v) -> (v, l)) variants) of
          Just label -> Right (TextBuilder.text label)
          Nothing ->
            Left
              Report
                { message = "Invalid enum value",
                  suggestion = "",
                  details =
                    [ ("type", tName),
                      ("expectedInputs", "[" <> Text.intercalate ", " (map fst variants) <> "]")
                    ]
                }

      textDecoder = do
        label <- Atto.takeText
        case lookup label variants of
          Just val -> pure val
          Nothing ->
            fail
              ( Text.unpack
                  ( "Unexpected enum value: "
                      <> label
                      <> ". Expected one of: "
                      <> Text.intercalate ", " (map fst variants)
                  )
              )
   in Scalar
        { schemaName = schema,
          typeName = tName,
          baseOid = Nothing,
          arrayOid = Nothing,
          supportsBinary = True,
          unresolvedTypes = Set.singleton (schema, tName),
          binaryEncoder = \_resolver -> encoder,
          binaryDecoder = \_resolver -> decoder,
          textualEncoder = \_resolver -> textEncoder,
          textualDecoder = \_resolver -> textDecoder
        }
