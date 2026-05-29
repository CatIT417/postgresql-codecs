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
    -- Decoding: wire format (inside composites/arrays).
    -- The PostgreSQL wire format represents NULL with a length prefix of @int32(-1)@.
    binaryDecoderWrapper ::
      PtrPeeker.Variable (Either DecodingError a) ->
      PtrPeeker.Variable (Either DecodingError b),
    textualDecoderWrapper ::
      Atto.Parser a ->
      Atto.Parser b,
    -- Decoding: surface format (top-level params/columns).
    -- These match driver APIs like libpq that use 'Maybe ByteString' / 'Maybe Text'.
    binarySurfaceDecoder ::
      (ByteString -> Either DecodingError a) ->
      Maybe ByteString ->
      Either DecodingError b,
    textualSurfaceDecoder ::
      (Text -> Either DecodingError a) ->
      Maybe Text ->
      Either DecodingError b,
    -- Encoding: surface format.
    -- 'Nothing' represents NULL. Callers in wire-format contexts (composites, arrays)
    -- resolve the 'Maybe' to @int32(-1)@ themselves.
    binaryEncoder ::
      (a -> Either EncodingError Write.Write) ->
      (b -> Either EncodingError (Maybe Write.Write)),
    textualEncoder ::
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
      textualDecoderWrapper = \contentDecoder -> do
        Atto.choice
          [ Atto.string "NULL" $> Nothing,
            Just <$> contentDecoder
          ],
      binarySurfaceDecoder = \run -> \case
        Nothing -> Right Nothing
        Just bs -> fmap Just (run bs),
      textualSurfaceDecoder = \run -> \case
        Nothing -> Right Nothing
        Just text -> fmap Just (run text),
      binaryEncoder = \contentEncoder -> \case
        Just a -> fmap Just (contentEncoder a)
        Nothing -> Right Nothing,
      textualEncoder = \contentEncoder -> \case
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
      textualDecoderWrapper = id,
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
      binaryEncoder = \contentEncoder a -> fmap Just (contentEncoder a),
      textualEncoder = \contentEncoder a -> fmap Just (contentEncoder a)
    }
