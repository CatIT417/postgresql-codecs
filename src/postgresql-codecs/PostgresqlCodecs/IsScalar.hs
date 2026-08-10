module PostgresqlCodecs.IsScalar where

import Data.Scientific (Scientific)
import qualified GHC.TypeLits as TypeLits
import PostgresqlCodecs.Core (Scalar)
import qualified PostgresqlCodecs.IsScalar.Scientific
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Scalar (binaryPrimitive, refineDecoding)
import qualified PostgresqlTypes as Types
import qualified PostgresqlTypes.Bool
import qualified PostgresqlTypes.Bytea
import qualified PostgresqlTypes.Date
import qualified PostgresqlTypes.Float4
import qualified PostgresqlTypes.Float8
import qualified PostgresqlTypes.Int2
import qualified PostgresqlTypes.Int4
import qualified PostgresqlTypes.Int8
import qualified PostgresqlTypes.Numeric
import qualified PostgresqlTypes.Text
import qualified PostgresqlTypes.Time
import qualified PostgresqlTypes.Timestamp
import qualified PostgresqlTypes.Timestamptz
import qualified PostgresqlTypes.Uuid

-- | Associates a Haskell type with its one canonical 'Scalar' codec.
--
-- 'PostgresqlCodecs.column', 'PostgresqlCodecs.param' and 'PostgresqlCodecs.field' require an
-- instance - there is no way to hand them a 'Scalar' value directly. This forces every Haskell
-- type used in a codec to have exactly one, named, global PostgreSQL representation, rather than
-- letting business logic about how to encode a type leak into ad-hoc 'Scalar' values scattered
-- across call sites. An instance is expected to be defined in terms of
-- 'PostgresqlCodecs.enum' or 'PostgresqlCodecs.Fields.composite'; for a type with a wire
-- format this library has no other constructor for, drop to the 'Scalar' record constructor
-- from "PostgresqlCodecs.Core" for raw authorship.
--
-- A type that needs more than one representation (e.g. the same enum mapped against two
-- different PostgreSQL schemas) should not share one Haskell type across both - a typeclass
-- instance is a single, global, coherence-checked choice per type, so it can't express that.
-- Define a distinct newtype per representation instead, each with its own instance.
-- 'PostgresqlCodecs.columnWith'\/'PostgresqlCodecs.paramWith'\/'PostgresqlCodecs.fieldWith'
-- remain as an explicit escape hatch taking a 'Scalar' value directly, but only for types with
-- no fixed static representation to hang an instance on (e.g. a codec resolved at runtime from
-- schema introspection) - never as a bypass for a type that already has, or should have, an
-- instance.
--
-- The instances shipped here come in two layers with a deliberately different contract. The
-- @postgresql-types@ wrapper types ('Types.Text', 'Types.Timestamptz', 'Types.Numeric', ...)
-- model the PostgreSQL domain exactly - invalidity is unrepresentable in the type rather than
-- merely detected, so their conversions are exact in both directions. The standard Haskell types
-- ('Text', 'UTCTime', 'Scientific', ...) are a convenience layer riding on those, and their
-- conversions are total but best-effort: anything the PostgreSQL type cannot represent gets
-- silently normalized - out-of-range values clamped, excess precision rounded, unrepresentable
-- characters dropped. Encoding a standard Haskell type therefore never fails, and never has to be
-- handled at the call site; the price is that a value can reach the database adjusted. Each such
-- instance documents its own normalization. When a column's exact domain matters, use the
-- corresponding wrapper type instead - that is what it is for.
--
-- There are, and deliberately can never be, instances for 'Maybe' or an array\/vector type:
-- nullability and dimensionality are chosen explicitly at each 'PostgresqlCodecs.column'\/
-- 'PostgresqlCodecs.param'\/'PostgresqlCodecs.field' call site, and 'Scalar' itself has no way to
-- represent "nullable" or "array of" - there is no field to fill in for such an instance.
class IsScalar a where
  scalar :: Scalar a

-- * Standard 'IsScalar' instances

--

-- $standardInstances
--
-- One instance per PostgreSQL built-in type shipped by @postgresql-types@, each deferring to
-- 'binaryPrimitive'. 'Types.Range' and 'Types.Multirange' get one instance per standard element
-- type (@int4@, @int8@, @numeric@, @date@, @timestamp@, @timestamptz@), mirroring the upstream
-- @IsRangeElement@\/@IsMultirangeElement@ instances; a range\/multirange over your own element
-- type needs an explicit 'Scalar'. Not covered: PostGIS's 'Types.Geometry' (not a
-- vanilla-PostgreSQL type). Construct a 'Scalar' for that with 'binaryPrimitive' directly.

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

-- | Maps to @int4range@.
instance IsScalar (Types.Range Types.Int4) where
  scalar = binaryPrimitive

-- | Maps to @int8range@.
instance IsScalar (Types.Range Types.Int8) where
  scalar = binaryPrimitive

-- | Maps to @numrange(precision, scale)@.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsScalar (Types.Range (Types.Numeric precision scale)) where
  scalar = binaryPrimitive

-- | Maps to @daterange@.
instance IsScalar (Types.Range Types.Date) where
  scalar = binaryPrimitive

-- | Maps to @tsrange@.
instance IsScalar (Types.Range Types.Timestamp) where
  scalar = binaryPrimitive

-- | Maps to @tstzrange@.
instance IsScalar (Types.Range Types.Timestamptz) where
  scalar = binaryPrimitive

-- | Maps to @int4multirange@.
instance IsScalar (Types.Multirange Types.Int4) where
  scalar = binaryPrimitive

-- | Maps to @int8multirange@.
instance IsScalar (Types.Multirange Types.Int8) where
  scalar = binaryPrimitive

-- | Maps to @nummultirange(precision, scale)@.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsScalar (Types.Multirange (Types.Numeric precision scale)) where
  scalar = binaryPrimitive

-- | Maps to @datemultirange@.
instance IsScalar (Types.Multirange Types.Date) where
  scalar = binaryPrimitive

-- | Maps to @tsmultirange@.
instance IsScalar (Types.Multirange Types.Timestamp) where
  scalar = binaryPrimitive

-- | Maps to @tstzmultirange@.
instance IsScalar (Types.Multirange Types.Timestamptz) where
  scalar = binaryPrimitive

-- * Standard Haskell type instances

--

-- $haskellInstances
--
-- A handful of common standard-library types get an instance too, each riding on the
-- corresponding @postgresql-types@ wrapper's instance above via 'invmap'. This is not an attempt
-- to cover every type in the section above with a plain-Haskell counterpart - types with no
-- single obvious Haskell representation (e.g. 'Types.Numeric', 'Types.Money', 'Types.Char' vs.
-- 'Types.Varchar' vs. 'Types.Bpchar' vs. 'Types.Citext' all wanting @Text@) are left to be
-- addressed with an explicitly named 'Scalar' instead.
--
-- 'Types.Range' and 'Types.Multirange' likewise get an instance for each of the plain Haskell
-- types above that has a range-element counterpart ('Int32', 'Int64', 'Day', 'LocalTime',
-- 'UTCTime'), riding on 'fmap' over the corresponding wrapper-typed range\/multirange instance.
--
-- 'Scientific' maps to unconstrained @numeric@ (@Types.Numeric 0 0@ - see its Haddock), the only
-- precision\/scale choice with no column-specific meaning to pick for it. A column declared with
-- an explicit @numeric(precision, scale)@ still needs 'Types.Numeric' directly. Its
-- 'Types.Range'\/'Types.Multirange' get the same 'fmap' treatment as the other range-element
-- types above.

-- | Maps to @bool@.
instance IsScalar Bool where
  scalar = invmap PostgresqlTypes.Bool.toBool PostgresqlTypes.Bool.fromBool binaryPrimitive

-- | Maps to @int2@.
instance IsScalar Int16 where
  scalar = invmap PostgresqlTypes.Int2.toInt16 PostgresqlTypes.Int2.fromInt16 binaryPrimitive

-- | Maps to @int4@.
instance IsScalar Int32 where
  scalar = invmap PostgresqlTypes.Int4.toInt32 PostgresqlTypes.Int4.fromInt32 binaryPrimitive

-- | Maps to @int8@.
instance IsScalar Int64 where
  scalar = invmap PostgresqlTypes.Int8.toInt64 PostgresqlTypes.Int8.fromInt64 binaryPrimitive

-- | Maps to @float4@.
instance IsScalar Float where
  scalar = invmap PostgresqlTypes.Float4.toFloat PostgresqlTypes.Float4.fromFloat binaryPrimitive

-- | Maps to @float8@.
instance IsScalar Double where
  scalar = invmap PostgresqlTypes.Float8.toDouble PostgresqlTypes.Float8.fromDouble binaryPrimitive

-- | Maps to @text@.
--
-- Encoding silently strips NUL characters, which @text@ cannot represent. Decoding is exact.
instance IsScalar Text where
  scalar = invmap PostgresqlTypes.Text.toText PostgresqlTypes.Text.normalizeFromText binaryPrimitive

-- | Maps to @bytea@.
instance IsScalar ByteString where
  scalar = invmap PostgresqlTypes.Bytea.toByteString PostgresqlTypes.Bytea.fromByteString binaryPrimitive

-- | Maps to @date@.
--
-- Encoding silently clamps dates outside @date@'s range (4713 BC to 5874897 AD). Decoding is
-- exact.
instance IsScalar Day where
  scalar = invmap PostgresqlTypes.Date.toDay PostgresqlTypes.Date.normalizeFromDay binaryPrimitive

-- | Maps to @time@.
--
-- Encoding silently rounds to microsecond resolution ('TimeOfDay' is picosecond-resolution) and
-- clamps to @time@'s range (@00:00:00@ to @24:00:00@). Decoding is exact.
instance IsScalar TimeOfDay where
  scalar = invmap PostgresqlTypes.Time.toTimeOfDay PostgresqlTypes.Time.normalizeFromTimeOfDay binaryPrimitive

-- | Maps to @timestamp@.
--
-- Encoding silently rounds to microsecond resolution ('LocalTime' is picosecond-resolution) and
-- clamps to @timestamp@'s range (4713 BC to 294276 AD). Decoding is exact.
instance IsScalar LocalTime where
  scalar = invmap PostgresqlTypes.Timestamp.toLocalTime PostgresqlTypes.Timestamp.normalizeFromLocalTime binaryPrimitive

-- | Maps to @timestamptz@.
--
-- Encoding silently rounds to microsecond resolution ('UTCTime' is picosecond-resolution) and
-- clamps to @timestamptz@'s range (4713 BC to 294276 AD). Decoding is exact.
--
-- The rounding matters in practice: 'Data.Time.Clock.getCurrentTime' routinely carries
-- sub-microsecond jitter, so a stored timestamp will usually differ from the value handed in.
instance IsScalar UTCTime where
  scalar = invmap PostgresqlTypes.Timestamptz.toUtcTime PostgresqlTypes.Timestamptz.normalizeFromUtcTime binaryPrimitive

-- | Maps to @uuid@.
instance IsScalar UUID where
  scalar = invmap PostgresqlTypes.Uuid.toUUID PostgresqlTypes.Uuid.fromUUID binaryPrimitive

-- | Maps to unconstrained @numeric@.
--
-- Encoding silently clamps to PostgreSQL's numeric limits (131072 digits before the decimal
-- point, 16383 after).
--
-- Decoding /fails/ on @NaN@, @Infinity@ and @-Infinity@: 'Scientific' can represent none of the
-- three, and there is no value to substitute that would not be a lie. This is the one place the
-- convenience layer rejects rather than normalizes, because the alternative is silent corruption
-- rather than an approximation. Decode as 'Types.Numeric' if the column can hold those values.
instance IsScalar Scientific where
  scalar =
    refineDecoding
      PostgresqlTypes.Numeric.normalizeFromScientific
      PostgresqlCodecs.IsScalar.Scientific.refineFromNumeric
      (binaryPrimitive @(Types.Numeric 0 0))

-- | Maps to @int4range@.
instance IsScalar (Types.Range Int32) where
  scalar = invmap (fmap PostgresqlTypes.Int4.toInt32) (fmap PostgresqlTypes.Int4.fromInt32) binaryPrimitive

-- | Maps to @int8range@.
instance IsScalar (Types.Range Int64) where
  scalar = invmap (fmap PostgresqlTypes.Int8.toInt64) (fmap PostgresqlTypes.Int8.fromInt64) binaryPrimitive

-- | Maps to @daterange@.
--
-- Each bound normalizes as described for 'Day'.
instance IsScalar (Types.Range Day) where
  scalar = invmap (fmap PostgresqlTypes.Date.toDay) (fmap PostgresqlTypes.Date.normalizeFromDay) binaryPrimitive

-- | Maps to @tsrange@.
--
-- Each bound normalizes as described for 'LocalTime'.
instance IsScalar (Types.Range LocalTime) where
  scalar = invmap (fmap PostgresqlTypes.Timestamp.toLocalTime) (fmap PostgresqlTypes.Timestamp.normalizeFromLocalTime) binaryPrimitive

-- | Maps to @tstzrange@.
--
-- Each bound normalizes as described for 'UTCTime'.
instance IsScalar (Types.Range UTCTime) where
  scalar = invmap (fmap PostgresqlTypes.Timestamptz.toUtcTime) (fmap PostgresqlTypes.Timestamptz.normalizeFromUtcTime) binaryPrimitive

-- | Maps to @numrange@ over unconstrained @numeric@.
--
-- Each bound encodes and decodes as described for 'Scientific' - in particular, a bound of
-- @NaN@, @Infinity@ or @-Infinity@ fails the decode of the whole range.
instance IsScalar (Types.Range Scientific) where
  scalar =
    refineDecoding
      (fmap PostgresqlTypes.Numeric.normalizeFromScientific)
      (traverse PostgresqlCodecs.IsScalar.Scientific.refineFromNumeric)
      (binaryPrimitive @(Types.Range (Types.Numeric 0 0)))

-- | Maps to @int4multirange@.
instance IsScalar (Types.Multirange Int32) where
  scalar = invmap (fmap PostgresqlTypes.Int4.toInt32) (fmap PostgresqlTypes.Int4.fromInt32) binaryPrimitive

-- | Maps to @int8multirange@.
instance IsScalar (Types.Multirange Int64) where
  scalar = invmap (fmap PostgresqlTypes.Int8.toInt64) (fmap PostgresqlTypes.Int8.fromInt64) binaryPrimitive

-- | Maps to @datemultirange@.
--
-- Each bound of each range normalizes as described for 'Day'.
instance IsScalar (Types.Multirange Day) where
  scalar = invmap (fmap PostgresqlTypes.Date.toDay) (fmap PostgresqlTypes.Date.normalizeFromDay) binaryPrimitive

-- | Maps to @tsmultirange@.
--
-- Each bound of each range normalizes as described for 'LocalTime'.
instance IsScalar (Types.Multirange LocalTime) where
  scalar = invmap (fmap PostgresqlTypes.Timestamp.toLocalTime) (fmap PostgresqlTypes.Timestamp.normalizeFromLocalTime) binaryPrimitive

-- | Maps to @tstzmultirange@.
--
-- Each bound of each range normalizes as described for 'UTCTime'.
instance IsScalar (Types.Multirange UTCTime) where
  scalar = invmap (fmap PostgresqlTypes.Timestamptz.toUtcTime) (fmap PostgresqlTypes.Timestamptz.normalizeFromUtcTime) binaryPrimitive

-- | Maps to @nummultirange@ over unconstrained @numeric@.
--
-- Each bound of each range encodes and decodes as described for 'Scientific' - in particular, a
-- bound of @NaN@, @Infinity@ or @-Infinity@ anywhere fails the decode of the whole multirange.
instance IsScalar (Types.Multirange Scientific) where
  scalar =
    refineDecoding
      (fmap PostgresqlTypes.Numeric.normalizeFromScientific)
      (traverse PostgresqlCodecs.IsScalar.Scientific.refineFromNumeric)
      (binaryPrimitive @(Types.Multirange (Types.Numeric 0 0)))
