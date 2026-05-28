{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Scalar where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import PostgresqlCodecs.DecodingError
import PostgresqlCodecs.EncodingError
import PostgresqlCodecs.Prelude
import qualified PostgresqlTypes.Algebra as PostgresqlTypes
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- TODO: Keep this constructor open for extensibility.
-- Or just provide a 'custom' constructor.
-- OTOH, keeping the constructor open also serves for portability:
-- it can be the point of integration with libraries interpreting it.
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
    binaryEncoder :: a -> Either EncodingError Write.Write,
    binaryDecoder :: PtrPeeker.Variable (Either DecodingError a),
    -- | Represent in Postgres textual format.
    textualEncoder :: a -> TextBuilder.TextBuilder,
    -- | Decode from Postgres textual format.
    textualDecoder :: Atto.Parser a
  }

instance Invariant Scalar where
  invmap f g Scalar {..} =
    Scalar
      { binaryEncoder = binaryEncoder . g,
        binaryDecoder = fmap (fmap f) binaryDecoder,
        textualEncoder = textualEncoder . g,
        textualDecoder = fmap f textualDecoder,
        ..
      }

-- | Lift a standard Postgres type into Scalar.
native :: forall a. (PostgresqlTypes.IsScalar a) => Scalar a
native =
  let -- Convert decoding errors between layers (they have identical shapes)
      convertError :: PostgresqlTypes.DecodingError -> DecodingError
      convertError PostgresqlTypes.DecodingError {location, reason} =
        case reason of
          PostgresqlTypes.ParsingDecodingErrorReason details input ->
            DecodingError
              { message = "Parsing error",
                details = details,
                row = Nothing,
                column = Nothing,
                inputLocation = location,
                inputOffset = 0,
                failingInput = Text.Encoding.decodeUtf8With (\_ _ -> Just '?') input,
                expectedInputs = Nothing
              }
          PostgresqlTypes.UnexpectedValueDecodingErrorReason expected actual ->
            DecodingError
              { message = "Unexpected value",
                details = "Expected: " <> expected <> ", got: " <> actual,
                row = Nothing,
                column = Nothing,
                inputLocation = location,
                inputOffset = 0,
                failingInput = actual,
                expectedInputs = Just [expected]
              }
          PostgresqlTypes.UnsupportedValueDecodingErrorReason details value ->
            DecodingError
              { message = "Unsupported value",
                details = details,
                row = Nothing,
                column = Nothing,
                inputLocation = location,
                inputOffset = 0,
                failingInput = value,
                expectedInputs = Nothing
              }
   in Scalar
        { schemaName = "",
          typeName = untag (PostgresqlTypes.typeName @a),
          baseOid = untag (PostgresqlTypes.baseOid @a),
          arrayOid = untag (PostgresqlTypes.arrayOid @a),
          binaryEncoder = \a -> Right (PostgresqlTypes.binaryEncoder a),
          binaryDecoder = fmap (first convertError) (PostgresqlTypes.binaryDecoder @a),
          textualEncoder = PostgresqlTypes.textualEncoder,
          textualDecoder = PostgresqlTypes.textualDecoder
        }

enum :: (Ord a) => Text -> Text -> [(Text, a)] -> Scalar a
enum schema tName variants =
  let byValue = Map.fromList (map (\(l, v) -> (v, l)) variants)

      -- Binary encoder: encode as text (enums are text in PostgreSQL wire protocol)
      encoder val =
        case Map.lookup val byValue of
          Just label -> Right (Write.textUtf8 label)
          Nothing ->
            Left
              EncodingError
                { message = "Unsupported enum value",
                  details = [("schema", schema), ("type", tName)],
                  param = 0,
                  inputLocation = [],
                  inputOffset = 0,
                  failingInput = "",
                  expectedInputs = Nothing
                }

      -- Binary decoder: decode from text and look up in variants map
      decoder = do
        bytes <- PtrPeeker.remainderAsByteString
        case Text.Encoding.decodeUtf8' bytes of
          Left e ->
            pure
              ( Left
                  DecodingError
                    { message = "Invalid UTF-8",
                      details = fromString (show e),
                      row = Nothing,
                      column = Nothing,
                      inputLocation = [],
                      inputOffset = 0,
                      failingInput = Text.Encoding.decodeUtf8With (\_ _ -> Just '?') bytes,
                      expectedInputs = Nothing
                    }
              )
          Right label ->
            case lookup label variants of
              Just val -> pure (Right val)
              Nothing ->
                pure
                  ( Left
                      DecodingError
                        { message = "Unexpected enum value",
                          details = "Expected one of: " <> Text.intercalate ", " (map fst variants),
                          row = Nothing,
                          column = Nothing,
                          inputLocation = [],
                          inputOffset = 0,
                          failingInput = label,
                          expectedInputs = Just (map fst variants)
                        }
                  )

      -- Textual encoder: just output the label
      textEncoder val =
        case lookup val (map (\(l, v) -> (v, l)) variants) of
          Just label -> TextBuilder.text label
          Nothing -> error ("Invalid enum value for type: " <> show tName)

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
          binaryEncoder = encoder,
          binaryDecoder = decoder,
          textualEncoder = textEncoder,
          textualDecoder = textDecoder
        }
