module PostgresqlCodecs.Nullability where

import PostgresqlCodecs.Core (Report)
import PostgresqlCodecs.Prelude

-- | Describes how NULL values are handled for a specific field or column.
-- @a@ is the non-null element type; @b@ is the decoded result type (e.g. @a@ or @Maybe a@).
data Nullability a b = Nullability
  { nullable :: Bool,
    onNull :: Either Report b,
    onNonNull :: a -> b,
    encoder ::
      forall c.
      -- Pack null.
      c ->
      -- Pack non-null.
      (a -> c) ->
      (b -> c)
  }

-- | Wraps decoded values in 'Maybe'; maps NULL to @Right Nothing@.
nullable :: Nullability a (Maybe a)
nullable =
  Nullability
    True
    (Right Nothing)
    Just
    maybe

-- | Rejects NULL with a 'Report' error; passes non-null values through unmodified.
nonNullable :: Nullability a a
nonNullable =
  Nullability
    False
    (Left "Unexpected null")
    id
    (const id)
