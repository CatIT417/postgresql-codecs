module PostgresqlCodecs.Nullability where

import PostgresqlCodecs.DecodingError
import PostgresqlCodecs.Prelude

data Nullability a b = Nullability
  { nullable :: Bool,
    onNull :: Either DecodingError b,
    onNonNull :: a -> Either DecodingError b,
    encoder :: forall c. (a -> c) -> (b -> Maybe c)
  }

nullable :: Nullability a (Maybe a)
nullable =
  Nullability
    True
    (Right Nothing)
    (\x -> Right (Just x))
    ( \pack ->
        \case
          Just x -> Just (pack x)
          Nothing -> Nothing
    )

nonNullable :: Nullability a a
nonNullable =
  Nullability
    False
    (Left "Unexpected null")
    Right
    ( \pack ->
        \x -> Just (pack x)
    )
