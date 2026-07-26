{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Scalar where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Report (Report (..))
import PostgresqlCodecs.Resolver
import qualified PostgresqlTypes.Algebra as PostgresqlTypes
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Full codec specification for a single PostgreSQL scalar type.
-- Carries the schema\/type name, optional statically-known OIDs, a binary-support
-- flag, and binary\/textual encoder\/decoder pairs.
data Scalar a = Scalar
  { -- | Schema name. If empty, the default schema will be used.
    schemaName :: Text,
    -- | Type name.
    typeName :: Text,
    -- | Statically known OID for the type.
    -- When unspecified, the OID may be determined at runtime by looking up by name.
    baseOid :: Maybe Word32,
    -- | Statically known OID for the array-type with this type as the element.
    -- When unspecified, the OID may be determined at runtime by looking up by name.
    -- It may also mean that there may be no array type containing this type, which is the case in attempts to double-nest arrays.
    arrayOid :: Maybe Word32,
    -- | Whether this scalar type supports binary encoding/decoding.
    supportsBinary :: Bool,
    -- | Transitive set of @(schema, name)@ pairs of referenced types whose base
    -- or array OID is not statically known and therefore must be resolved at runtime.
    unresolvedTypes :: Set (Text, Text),
    binaryEncoder :: Resolver -> a -> Either Report Write.Write,
    binaryDecoder :: Resolver -> PtrPeeker.Variable (Either Report a),
    -- | Represent in Postgres textual format.
    textualEncoder :: Resolver -> a -> Either Report TextBuilder.TextBuilder,
    -- | Decode from Postgres textual format.
    textualDecoder :: Resolver -> Atto.Parser a
  }

instance Invariant Scalar where
  invmap f g Scalar {..} =
    Scalar
      { binaryEncoder = \r -> binaryEncoder r . g,
        binaryDecoder = \r -> fmap (fmap f) (binaryDecoder r),
        textualEncoder = \r -> textualEncoder r . g,
        textualDecoder = \r -> fmap f (textualDecoder r),
        ..
      }

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

-- | Build a 'Scalar' from raw codec functions, for types that don't fit 'standard' or 'enum' —
-- e.g. a type whose binary wire format the server doesn't support, or one this library has no
-- other constructor for.
custom ::
  -- | Schema name. Empty for the default schema.
  Text ->
  -- | Type name.
  Text ->
  -- | Statically known base OID, if any.
  Maybe Word32 ->
  -- | Statically known array OID, if any.
  Maybe Word32 ->
  -- | Whether the type supports binary encoding/decoding.
  Bool ->
  (Resolver -> a -> Either Report Write.Write) ->
  (Resolver -> PtrPeeker.Variable (Either Report a)) ->
  (Resolver -> a -> Either Report TextBuilder.TextBuilder) ->
  (Resolver -> Atto.Parser a) ->
  Scalar a
custom schemaName typeName baseOid arrayOid supportsBinary binaryEncoder binaryDecoder textualEncoder textualDecoder =
  Scalar
    { schemaName,
      typeName,
      baseOid,
      arrayOid,
      supportsBinary,
      unresolvedTypes =
        if isNothing baseOid || isNothing arrayOid
          then Set.singleton (schemaName, typeName)
          else mempty,
      binaryEncoder,
      binaryDecoder,
      textualEncoder,
      textualDecoder
    }

-- | Derive a 'Scalar' from a standard @postgresql-types@ 'PostgresqlTypes.IsPrimitive' instance.
standard :: forall a. (PostgresqlTypes.IsPrimitive a, PostgresqlTypes.IsBinaryPrimitive a) => Scalar a
standard =
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
   in Scalar
        { schemaName = fromMaybe "" (untag (PostgresqlTypes.schemaName @a)),
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
              then Set.singleton ("", untag (PostgresqlTypes.typeName @a))
              else mempty,
          binaryEncoder = \_resolver a -> Right (PostgresqlTypes.binaryEncoder a),
          binaryDecoder = \_resolver -> fmap (first convertError) (PostgresqlTypes.binaryDecoder @a),
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
