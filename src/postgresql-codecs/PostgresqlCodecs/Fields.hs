module PostgresqlCodecs.Fields where

import qualified Data.Attoparsec.Text as Atto
import PostgresqlCodecs.DecodingError
import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.EncodingError
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Scalar
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data Fields a b = Fields
  { count :: Int32,
    binaryEncoder :: a -> Either EncodingError Write.Write,
    binaryDecoder :: PtrPeeker.Variable (Either DecodingError b),
    -- | Represent in Postgres textual format.
    textualEncoder :: a -> Either EncodingError [TextBuilder.TextBuilder],
    textualDecoder :: Atto.Parser b
  }

instance Functor (Fields a) where
  fmap f Fields {..} =
    Fields
      { count,
        binaryEncoder,
        binaryDecoder = fmap (fmap f) binaryDecoder,
        textualEncoder,
        textualDecoder = fmap f textualDecoder
      }

instance Applicative (Fields a) where
  pure x =
    Fields
      { count = 0,
        binaryEncoder = const (Right mempty),
        binaryDecoder = pure (Right x),
        textualEncoder = const (Right []),
        textualDecoder = pure x
      }
  (<*>) left right =
    Fields
      { count = left.count + right.count,
        binaryEncoder = \value -> left.binaryEncoder value <> right.binaryEncoder value,
        binaryDecoder = liftA2 (<*>) left.binaryDecoder right.binaryDecoder,
        textualEncoder = \value -> do
          leftTexts <- left.textualEncoder value
          rightTexts <- right.textualEncoder value
          Right (leftTexts <> rightTexts),
        textualDecoder = error "TODO: Merge with care taken about multiple fields"
      }

instance Profunctor Fields where
  dimap f g Fields {..} =
    Fields
      { count,
        binaryEncoder = \value -> binaryEncoder (f value),
        binaryDecoder = fmap (fmap g) binaryDecoder,
        textualEncoder = \value -> textualEncoder (f value),
        textualDecoder = fmap g textualDecoder
      }

field :: Scalar a -> Dimensionality a b -> Nullability b c -> Fields c c
field scalar dimensionality nullability =
  let baseOid = fromMaybe 705 scalar.baseOid
      binEnc = dimensionality.binaryEncoder baseOid scalar.binaryEncoder
      txtEnc = dimensionality.textualEncoder scalar.textualEncoder
   in Fields
        { count = 1,
          binaryEncoder = \value -> do
            mWrite <- nullability.binaryEncoder binEnc value
            let oid = Write.bInt32 (fromIntegral baseOid)
            case mWrite of
              Nothing -> Right (oid <> Write.bInt32 (-1))
              Just write ->
                let size = Write.bInt32 (fromIntegral (Write.writeSize write))
                 in Right (oid <> size <> write),
          binaryDecoder = do
            contentSize <- PtrPeeker.fixed PtrPeeker.beSignedInt4
            case contentSize of
              -1 -> pure nullability.onNull
              _ ->
                fmap
                  (>>= nullability.onNonNull)
                  (dimensionality.binaryDecoder baseOid scalar.binaryDecoder),
          textualEncoder = \value -> do
            mText <- nullability.textualEncoder txtEnc value
            case mText of
              Nothing -> Right [mempty]
              Just text -> Right [text],
          textualDecoder =
            error "TODO: Implement decoding of a composite field. Take care about nesting"
        }

composite :: Text -> Text -> Fields a a -> Scalar a
composite schemaName typeName fields =
  Scalar
    { schemaName,
      typeName,
      baseOid = Nothing,
      arrayOid = Nothing,
      binaryEncoder = \value -> do
        fieldWrites <- fields.binaryEncoder value
        Right (Write.lInt32 fields.count <> fieldWrites),
      binaryDecoder =
        runExceptT do
          _ <-
            ExceptT do
              PtrPeeker.fixed do
                PtrPeeker.beSignedInt4
                  <&> \count ->
                    if count == fields.count
                      then Right ()
                      else
                        Left
                          DecodingError
                            { message = "Field count mismatch",
                              details = "Expected " <> TextBuilder.toText (TextBuilder.decimal fields.count) <> " fields, but got " <> TextBuilder.toText (TextBuilder.decimal count),
                              row = Nothing,
                              column = Nothing,
                              inputLocation = ["field-count"],
                              inputOffset = 0,
                              failingInput = TextBuilder.toText (TextBuilder.decimal count),
                              expectedInputs = Just [TextBuilder.toText (TextBuilder.decimal fields.count)]
                            }
          ExceptT fields.binaryDecoder,
      textualEncoder = \_value -> error "TODO",
      textualDecoder = error "TODO"
    }
