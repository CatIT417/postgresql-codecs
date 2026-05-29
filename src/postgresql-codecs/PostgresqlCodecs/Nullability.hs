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
    -- Wire format wrappers (inside composites/arrays).
    -- The PostgreSQL wire format represents NULL with a length prefix of @int32(-1)@.
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
      (b -> Either EncodingError TextBuilder.TextBuilder),
    -- Surface format wrappers (top-level params/columns).
    -- These match driver APIs like libpq that use 'Maybe ByteString' / 'Maybe Text'.
    binarySurfaceDecoder ::
      (ByteString -> Either DecodingError a) ->
      Maybe ByteString ->
      Either DecodingError b,
    binarySurfaceEncoder ::
      (a -> Either EncodingError Write.Write) ->
      (b -> Either EncodingError (Maybe Write.Write)),
    textualSurfaceDecoder ::
      (Text -> Either DecodingError a) ->
      Maybe Text ->
      Either DecodingError b,
    textualSurfaceEncoder ::
      (a -> Either EncodingError TextBuilder.TextBuilder) ->
      (b -> Either EncodingError (Maybe TextBuilder.TextBuilder))
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
          ],
      -- Surface
      binarySurfaceDecoder = \run -> \case
        Nothing -> Right Nothing
        Just bs -> fmap Just (run bs),
      binarySurfaceEncoder = \contentEncoder -> \case
        Just a -> fmap Just (contentEncoder a)
        Nothing -> Right Nothing,
      textualSurfaceDecoder = \run -> \case
        Nothing -> Right Nothing
        Just text -> fmap Just (run text),
      textualSurfaceEncoder = \contentEncoder -> \case
        Just a -> fmap Just (contentEncoder a)
        Nothing -> Right Nothing
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
      textualDecoderWrapper = id,
      -- Surface
      binarySurfaceDecoder = \run -> \case
        Nothing ->
          Left
            DecodingError
              { message = "Unexpected NULL value",
                details = "Expected non-nullable value, but got NULL",
                row = Nothing,
                column = Nothing,
                inputLocation = ["surface-nullability"],
                inputOffset = 0,
                failingInput = "",
                expectedInputs = Nothing
              }
        Just bs -> run bs,
      binarySurfaceEncoder = \contentEncoder a -> fmap Just (contentEncoder a),
      textualSurfaceDecoder = \run -> \case
        Nothing ->
          Left
            DecodingError
              { message = "Unexpected NULL value",
                details = "Expected non-nullable value, but got NULL",
                row = Nothing,
                column = Nothing,
                inputLocation = ["surface-nullability"],
                inputOffset = 0,
                failingInput = "",
                expectedInputs = Nothing
              }
        Just text -> run text,
      textualSurfaceEncoder = \contentEncoder a -> fmap Just (contentEncoder a)
    }
