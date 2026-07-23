{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Resolver where

import PostgresqlCodecs.Prelude

-- | Base type OID and array-type OID for a PostgreSQL type. 0 means "unknown".
data TypeOids = TypeOids
  { baseOid :: Word32,
    arrayOid :: Word32
  }
  deriving stock (Eq, Show)

-- | Resolves a (schema, type-name) pair to its 'TypeOids'.
-- First arg = schema name ("" = default schema, resolved via search_path); second = type name.
-- Returns 0 for any OID it cannot determine.
type Resolver = Text -> Text -> TypeOids
