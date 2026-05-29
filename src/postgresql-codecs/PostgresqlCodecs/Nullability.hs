module PostgresqlCodecs.Nullability where

import PostgresqlCodecs.DecodingError
import PostgresqlCodecs.EncodingError
import PostgresqlCodecs.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data Nullability a b = Nullability
  { nullable :: Bool,
    onNull :: Either DecodingError b,
    onNonNull :: a -> Either DecodingError b,
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
    True
    (Right Nothing)
    (\x -> Right (Just x))
    ( \contentEncoder ->
        \case
          Just x -> fmap Just (contentEncoder x)
          Nothing -> Right Nothing
    )
    ( \contentEncoder ->
        \case
          Just x -> fmap Just (contentEncoder x)
          Nothing -> Right Nothing
    )

nonNullable :: Nullability a a
nonNullable =
  Nullability
    False
    (Left "Unexpected null")
    Right
    ( \contentEncoder ->
        \x -> fmap Just (contentEncoder x)
    )
    ( \contentEncoder ->
        \x -> fmap Just (contentEncoder x)
    )
