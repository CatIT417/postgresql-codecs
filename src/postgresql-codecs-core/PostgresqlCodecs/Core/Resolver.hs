{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Core.Resolver where

import PostgresqlCodecs.Core.Prelude

-- | Base type OID and array-type OID for a PostgreSQL type.
data TypeOids = TypeOids
  { -- | OID of the base (non-array) type. @0@ means unknown.
    baseOid :: Word32,
    -- | OID of the array type whose element is the base type. @0@ means
    -- unknown; it may also legitimately be @0@ when the type has no array
    -- type (as with a double-nested array element).
    arrayOid :: Word32
  }
  deriving stock (Eq, Show)

-- | Resolves a @(schemaName, typeName)@ pair — an empty schema name meaning
-- the default schema, resolved via @search_path@ — to its 'TypeOids'.
--
-- A driver builds exactly one 'Resolver' per statement, typically backed by
-- a single catalogue lookup (e.g. via @to_regtype@) for the union of every
-- codec's @unresolvedTypes@ involved. See "PostgresqlCodecs.Core" for the full
-- driver contract.
--
-- 'Resolver' is total: it returns @TypeOids 0 0@ for anything it cannot
-- resolve, rather than failing. For a scalar slot that degrades gracefully —
-- PostgreSQL treats @0@ as its \"unspecified\" sentinel and either infers the
-- type itself or reports a server-side error naming it. A binary array of an
-- unresolved type, however, cannot be decoded at all: the array-header check
-- compares the expected element OID against the one the server sends, so an
-- expected @0@ fails with a @Base OID mismatch@ report. See
-- "PostgresqlCodecs.Core" for the full contract.
--
-- Drivers should cache resolutions — the natural granularity is per
-- connection, since the OIDs of user-defined types are stable within a
-- database but differ across databases.
type Resolver = Text -> Text -> TypeOids
