module PostgresqlCodecs.Nullability where

import qualified Data.Attoparsec.Text as Atto
import PostgresqlCodecs.DecodingError
import PostgresqlCodecs.EncodingError
import PostgresqlCodecs.Prelude
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data Nullability a b = Nullability
  { nullable :: Bool,
    binaryDecoderWrapper ::
      PtrPeeker.Variable (Either DecodingError a) ->
      PtrPeeker.Variable (Either DecodingError b),
    binaryEncoderWrapper ::
      (a -> Either EncodingError Write.Write) ->
      (b -> Either EncodingError Write.Write),
    textualDecoderWrapper ::
      Atto.Parser a ->
      Atto.Parser b,
    textualEncoderWrapper ::
      (a -> Either EncodingError TextBuilder.TextBuilder) ->
      (b -> Either EncodingError TextBuilder.TextBuilder)
  }

nullable :: Nullability a (Maybe a)
nullable =
  Nullability
    { nullable = True,
      binaryDecoderWrapper = \contentDecoder -> do
        contentSize <- PtrPeeker.fixed PtrPeeker.beSignedInt4
        case contentSize of
          -1 ->
            pure (Right Nothing)
          _ ->
            fmap (fmap Just) contentDecoder,
      binaryEncoderWrapper = \contentEncoder -> \case
        Just a -> do
          content <- contentEncoder a
          let contentSize = Write.bInt32 (fromIntegral (Write.writeSize content))
          Right (contentSize <> content)
        Nothing -> Right (Write.bInt32 (-1)),
      textualEncoderWrapper = \contentEncoder -> \case
        Just a -> contentEncoder a
        Nothing -> Right "NULL",
      textualDecoderWrapper = \contentDecoder -> do
        Atto.choice
          [ Atto.string "NULL" $> Nothing,
            Just <$> contentDecoder
          ]
    }

nonNullable :: Nullability a a
nonNullable =
  Nullability
    { nullable = False,
      binaryDecoderWrapper = \contentDecoder -> do
        contentSize <- PtrPeeker.fixed PtrPeeker.beSignedInt4
        case contentSize of
          -1 ->
            pure
              ( Left
                  DecodingError
                    { message = "Unexpected NULL value",
                      details = "Expected non-nullable value, but got NULL",
                      row = Nothing,
                      column = Nothing,
                      inputLocation = ["content-nullability"],
                      inputOffset = 0,
                      failingInput = "",
                      expectedInputs = Nothing
                    }
              )
          _ ->
            contentDecoder,
      binaryEncoderWrapper = \contentEncoder a -> do
        content <- contentEncoder a
        let contentSize = Write.bInt32 (fromIntegral (Write.writeSize content))
        Right (contentSize <> content),
      textualEncoderWrapper = id,
      textualDecoderWrapper = id
    }
