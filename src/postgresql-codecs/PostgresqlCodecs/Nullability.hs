module PostgresqlCodecs.Nullability where

import PostgresqlCodecs.DecodingError
import PostgresqlCodecs.Prelude

data Nullability a b = Nullability
  { nullable :: Bool,
    onNull :: Either DecodingError b,
    onNonNull :: a -> Either DecodingError b,
    encoder ::
      forall c.
      -- Pack null.
      c ->
      -- Pack non-null.
      (a -> c) ->
      (b -> c)
  }

nullable :: Nullability a (Maybe a)
nullable =
  Nullability
    True
    (Right Nothing)
    (\x -> Right (Just x))
    ( \nullVal pack ->
        \case
          Just x -> pack x
          Nothing -> nullVal
    )

nonNullable :: Nullability a a
nonNullable =
  Nullability
    False
    (Left "Unexpected null")
    Right
    (\_nullVal pack -> pack)
