module PostgresqlCodecs.Fields where

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
    textualEncoder :: a -> Either EncodingError [TextBuilder.TextBuilder]
  }

instance Functor (Fields a) where
  fmap f Fields {..} =
    Fields
      { count,
        binaryEncoder,
        binaryDecoder = fmap (fmap f) binaryDecoder,
        textualEncoder
      }

instance Applicative (Fields a) where
  pure x =
    Fields
      { count = 0,
        binaryEncoder = const (Right mempty),
        binaryDecoder = pure (Right x),
        textualEncoder = const (Right [])
      }
  (<*>) left right =
    Fields
      { count = left.count + right.count,
        binaryEncoder = \value -> left.binaryEncoder value <> right.binaryEncoder value,
        binaryDecoder = liftA2 (<*>) left.binaryDecoder right.binaryDecoder,
        textualEncoder = \value -> do
          leftTexts <- left.textualEncoder value
          rightTexts <- right.textualEncoder value
          Right (leftTexts <> rightTexts)
      }

instance Profunctor Fields where
  dimap f g Fields {..} =
    Fields
      { count,
        binaryEncoder = \value -> binaryEncoder (f value),
        binaryDecoder = fmap (fmap g) binaryDecoder,
        textualEncoder = \value -> textualEncoder (f value)
      }

field :: Scalar a -> Dimensionality a b -> Nullability b c -> Fields c c
field scalar dimensionality nullability =
  let baseOid = fromMaybe 705 scalar.baseOid
   in Fields
        { count = 1,
          binaryEncoder =
            nullability.binaryEncoderWrapper
              (dimensionality.binaryEncoder baseOid scalar.binaryEncoder),
          binaryDecoder =
            nullability.binaryDecoderWrapper
              (dimensionality.binaryDecoder baseOid scalar.binaryDecoder),
          textualEncoder =
            fmap pure
              . nullability.textualEncoderWrapper
                (dimensionality.textualEncoder scalar.textualEncoder)
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
