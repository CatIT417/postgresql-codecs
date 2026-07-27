module PostgresqlCodecs.IsScalar where

import qualified GHC.TypeLits as TypeLits
import PostgresqlCodecs.Scalar
import qualified PostgresqlTypes as Types

-- | Associates a Haskell type with its one canonical 'Scalar' codec.
--
-- This is purely an opt-in convenience for the common case of "this type has exactly one obvious
-- PostgreSQL representation". It does not replace 'PostgresqlCodecs.Scalar.custom',
-- 'PostgresqlCodecs.Scalar.enum', 'PostgresqlCodecs.Fields.composite',
-- 'PostgresqlCodecs.Scalar.binaryPrimitive' or 'PostgresqlCodecs.Scalar.textualPrimitive' — those
-- remain the way to build a 'Scalar' value explicitly, and are what an instance's 'scalar' method
-- is expected to be defined in terms of.
--
-- Reach for an instance when a type has one obvious mapping (most enums and composites); keep
-- passing a named 'Scalar' value explicitly, without an instance, when a type needs more than
-- one representation (e.g. the same enum mapped against two different PostgreSQL schemas, or a
-- variant built for a test) — a typeclass instance is a single, global, coherence-checked choice
-- per type, so it can't express that.
--
-- There are, and deliberately can never be, instances for 'Maybe' or an array\/vector type:
-- nullability and dimensionality are chosen explicitly at each 'PostgresqlCodecs.column'\/
-- 'PostgresqlCodecs.param'\/'PostgresqlCodecs.field' call site, and 'Scalar' itself has no way to
-- represent "nullable" or "array of" — there is no field to fill in for such an instance.
class IsScalar a where
  scalar :: Scalar a

-- * Standard 'IsScalar' instances

--

-- $standardInstances
--
-- One instance per PostgreSQL built-in type shipped by @postgresql-types@, each deferring to
-- 'binaryPrimitive'. Not covered: 'Types.Range' and 'Types.Multirange' (generic over an element
-- type, with no single obvious instance to write) and PostGIS's 'Types.Geometry' (not a
-- vanilla-PostgreSQL type). Construct 'Scalar's for those with 'binaryPrimitive' directly.

-- | Maps to @int2@.
instance IsScalar Types.Int2 where
  scalar = binaryPrimitive

-- | Maps to @int4@.
instance IsScalar Types.Int4 where
  scalar = binaryPrimitive

-- | Maps to @int8@.
instance IsScalar Types.Int8 where
  scalar = binaryPrimitive

-- | Maps to @float4@.
instance IsScalar Types.Float4 where
  scalar = binaryPrimitive

-- | Maps to @float8@.
instance IsScalar Types.Float8 where
  scalar = binaryPrimitive

-- | Maps to @numeric(precision, scale)@.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsScalar (Types.Numeric precision scale) where
  scalar = binaryPrimitive

-- | Maps to @money@.
instance IsScalar Types.Money where
  scalar = binaryPrimitive

-- | Maps to @oid@.
instance IsScalar Types.Oid where
  scalar = binaryPrimitive

-- | Maps to @text@.
instance IsScalar Types.Text where
  scalar = binaryPrimitive

-- | Maps to @varchar(maxLen)@.
instance (TypeLits.KnownNat maxLen) => IsScalar (Types.Varchar maxLen) where
  scalar = binaryPrimitive

-- | Maps to @char@.
instance IsScalar Types.Char where
  scalar = binaryPrimitive

-- | Maps to @bpchar(numChars)@.
instance (TypeLits.KnownNat numChars) => IsScalar (Types.Bpchar numChars) where
  scalar = binaryPrimitive

-- | Maps to @citext@.
instance IsScalar Types.Citext where
  scalar = binaryPrimitive

-- | Maps to @bool@.
instance IsScalar Types.Bool where
  scalar = binaryPrimitive

-- | Maps to @bytea@.
instance IsScalar Types.Bytea where
  scalar = binaryPrimitive

-- | Maps to @date@.
instance IsScalar Types.Date where
  scalar = binaryPrimitive

-- | Maps to @time@.
instance IsScalar Types.Time where
  scalar = binaryPrimitive

-- | Maps to @timestamp@.
instance IsScalar Types.Timestamp where
  scalar = binaryPrimitive

-- | Maps to @timestamptz@.
instance IsScalar Types.Timestamptz where
  scalar = binaryPrimitive

-- | Maps to @timetz@.
instance IsScalar Types.Timetz where
  scalar = binaryPrimitive

-- | Maps to @interval@.
instance IsScalar Types.Interval where
  scalar = binaryPrimitive

-- | Maps to @inet@.
instance IsScalar Types.Inet where
  scalar = binaryPrimitive

-- | Maps to @cidr@.
instance IsScalar Types.Cidr where
  scalar = binaryPrimitive

-- | Maps to @macaddr@.
instance IsScalar Types.Macaddr where
  scalar = binaryPrimitive

-- | Maps to @macaddr8@.
instance IsScalar Types.Macaddr8 where
  scalar = binaryPrimitive

-- | Maps to @point@.
instance IsScalar Types.Point where
  scalar = binaryPrimitive

-- | Maps to @line@.
instance IsScalar Types.Line where
  scalar = binaryPrimitive

-- | Maps to @lseg@.
instance IsScalar Types.Lseg where
  scalar = binaryPrimitive

-- | Maps to @box@.
instance IsScalar Types.Box where
  scalar = binaryPrimitive

-- | Maps to @path@.
instance IsScalar Types.Path where
  scalar = binaryPrimitive

-- | Maps to @polygon@.
instance IsScalar Types.Polygon where
  scalar = binaryPrimitive

-- | Maps to @circle@.
instance IsScalar Types.Circle where
  scalar = binaryPrimitive

-- | Maps to @bit(numBits)@.
instance (TypeLits.KnownNat numBits) => IsScalar (Types.Bit numBits) where
  scalar = binaryPrimitive

-- | Maps to @varbit(maxLen)@.
instance (TypeLits.KnownNat maxLen) => IsScalar (Types.Varbit maxLen) where
  scalar = binaryPrimitive

-- | Maps to @uuid@.
instance IsScalar Types.Uuid where
  scalar = binaryPrimitive

-- | Maps to @json@.
instance IsScalar Types.Json where
  scalar = binaryPrimitive

-- | Maps to @jsonb@.
instance IsScalar Types.Jsonb where
  scalar = binaryPrimitive

-- | Maps to @hstore@.
instance IsScalar Types.Hstore where
  scalar = binaryPrimitive

-- | Maps to @tsvector@.
instance IsScalar Types.Tsvector where
  scalar = binaryPrimitive
