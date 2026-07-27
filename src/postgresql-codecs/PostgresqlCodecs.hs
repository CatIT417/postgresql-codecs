-- | Driver-agnostic PostgreSQL codec library.
--
-- Provides a five-layer abstraction for encoding and decoding PostgreSQL values:
--
-- 1. 'Scalar' — a single PostgreSQL scalar type with binary\/textual encoders and decoders.
-- 2. 'Dimensionality' — pass-through ('d0') or N-dimensional array wrapping ('d1'–'d6').
-- 3. 'Nullability' — whether values may be NULL ('nullable' or 'nonNullable').
-- 4. 'Columns' \/ 'Params' — result-set column decoder and query parameter encoder.
-- 5. 'Result' — full result-set decoding strategy.
--
-- = Associating a type with its 'Scalar'
--
-- 'IsScalar' is an optional convenience class: @instance IsScalar T where scalar = ...@ lets
-- 'field' \/ 'column' \/ 'param' pick up @T@'s 'Scalar' from context instead of a named value
-- being threaded through by hand. Every standard PostgreSQL type has an instance already. Write
-- one for your own enum\/composite types when they have exactly one obvious representation; when
-- a type needs more than one (e.g. mapped against two different schemas), keep naming and
-- passing 'Scalar' values explicitly instead — a class instance is a single, coherence-checked,
-- global choice per type. See 'IsScalar' for the full reasoning, including why it can never have
-- an instance for 'Maybe' or an array\/vector type.
--
-- = Resolving type OIDs
--
-- Codecs built from 'enum' or 'composite', and 'binaryPrimitive'\/'textualPrimitive' scalars
-- whose @IsPrimitive@ instance has no static OID (e.g. @citext@, @hstore@), reference
-- PostgreSQL types whose OIDs this library cannot know statically. Such a
-- codec advertises the types it needs resolved via its
-- @unresolvedTypes :: Set (Text, Text)@ field, a set of @(schemaName,
-- typeName)@ pairs. An empty @schemaName@ denotes the default schema, to be
-- resolved through the server's @search_path@. A driver author owes the
-- library the following:
--
-- 1. Before executing a statement, collect the union of the
--    @unresolvedTypes@ of everything about to be used — for this library's
--    shape that is @params.unresolvedTypes <> Result.unresolvedTypes
--    result@. An empty set means zero catalogue round-trips, which is the
--    common case for codecs built only from statically-known 'binaryPrimitive'
--    or 'textualPrimitive' scalars.
-- 2. If that set is non-empty, look the types up in the server catalogue
--    (e.g. via @to_regtype@) and build ONE 'Resolver' from the result.
-- 3. Pass that single 'Resolver' to every resolver-parameterized field
--    touched for the statement: @Columns.decoder@, @Columns.oids@,
--    @Params.oids@, and @Params.binaryEncoder@\/@textualEncoder@.
-- 4. Understand what an unresolved type costs. 'Resolver' is total: it
--    returns @TypeOids 0 0@ for anything it cannot resolve, @0@ being
--    PostgreSQL's \"unspecified\" sentinel. For a scalar slot that degrades
--    gracefully — the server infers the type or reports an error naming it.
--    For a binary array it does not: the library's own array-header check
--    compares the expected element OID against the one the server sends, so
--    an expected @0@ fails the decode with a @Base OID mismatch@ @Report@.
--    Resolution is therefore mandatory, not best-effort, for a binary
--    array-typed codec.
-- 5. Cache resolutions, ideally per connection (OIDs of user-defined types
--    are stable within a database but differ across databases). The
--    integration test harness in this repository re-queries @pg_type@ on
--    every query for simplicity; that is NOT the pattern to copy for
--    performance.
--
-- The textual wire format embeds no OIDs, so the textual encoders and
-- decoders ignore their 'Resolver' argument; they still take one so drivers
-- can treat both wire formats uniformly. This applies to the codecs only —
-- a textual parameter is still declared to the server by OID, so
-- @Params.oids@ needs a resolver regardless of wire format.
module PostgresqlCodecs
  ( -- * Report
    Report (..),

    -- * Resolver
    Resolver,
    TypeOids (..),

    -- * Parameter
    Params (..),
    Format (..),
    param,

    -- * Result sets
    Result (..),
    oneRow,
    someRow,
    manyRows,
    rowsAffected,
    refine,
    unresolvedTypes,

    -- ** Result set columns
    Columns (..),
    ConsumesColumns (..),
    column,

    -- * Nullability
    Nullability,
    nonNullable,
    nullable,

    -- * Dimensionality
    Dimensionality,
    d0,
    d1,
    d2,
    d3,
    d4,
    d5,
    d6,

    -- ** Dimension
    Dimension,
    vector,

    -- * Scalar types
    Scalar (..),
    IsScalar (..),
    binaryPrimitive,
    textualPrimitive,
    custom,
    enum,
    composite,

    -- * Composite types
    Fields,
    field,

    -- * Standard types
    module PostgresqlTypes,
  )
where

import PostgresqlCodecs.Columns
import PostgresqlCodecs.Dimension
import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.Fields
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Params
import PostgresqlCodecs.Report (Report (..))
import PostgresqlCodecs.Resolver
import PostgresqlCodecs.Result
import PostgresqlCodecs.Scalar
import PostgresqlTypes
