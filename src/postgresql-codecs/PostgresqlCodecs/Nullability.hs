module PostgresqlCodecs.Nullability where

import PostgresqlCodecs.DecodingError
import PostgresqlCodecs.Prelude

data Nullability a b = Nullability
  { nullable :: Bool,
    onNull :: Either DecodingError b,
    onNonNull :: a -> b,
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
    Just
    maybe

nonNullable :: Nullability a a
nonNullable =
  Nullability
    False
    (Left "Unexpected null")
    id
    (const id)
