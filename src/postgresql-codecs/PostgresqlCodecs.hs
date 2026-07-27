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
-- The codec /types/ ('Scalar', 'Params', 'Columns', 'Result') are re-exported here
-- /abstractly/ — without their constructors — so application type signatures compile
-- without importing core. /Driver authors/ who need to deconstruct those records, and
-- who must fulfill the OID-resolution contract, import "PostgresqlCodecs.Core" instead;
-- that module documents the driver contract in full.
--
-- = Associating a type with its 'Scalar'
--
-- 'field' \/ 'column' \/ 'param' require an @instance IsScalar T where scalar = ...@ for @T@ —
-- there is no way to hand them a 'Scalar' value directly. This is deliberate: it forces every
-- Haskell type used in a codec to have exactly one canonical PostgreSQL representation, named
-- once, globally, instead of business logic about how to encode a type leaking into ad-hoc
-- 'Scalar' values scattered across call sites. Every standard PostgreSQL type has an instance
-- already. Write one for your own enum\/composite types. A type that legitimately needs more
-- than one representation (e.g. mapped against two different schemas) should not share one
-- Haskell type across both — define a distinct newtype per representation, each with its own
-- instance.
--
-- 'columnWith' \/ 'paramWith' \/ 'fieldWith' take a 'Scalar' directly. Reach for them only when
-- there is no static Haskell type to hang an instance on — e.g. a codec resolved at runtime from
-- schema introspection — never as a substitute for an instance a type already has or should have.
-- See 'IsScalar' for the full reasoning.
module PostgresqlCodecs
  ( -- * Report
    Report (..),

    -- * Parameter
    Params,
    param,
    paramWith,

    -- * Result sets
    Result,
    oneRow,
    someRow,
    manyRows,
    rowsAffected,
    refine,

    -- ** Result set columns
    Columns,
    column,
    columnWith,

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
    Scalar,
    IsScalar (..),
    enum,
    composite,

    -- * Composite types
    Fields,
    field,
    fieldWith,

    -- * Standard types
    module PostgresqlTypes,
  )
where

import PostgresqlCodecs.Columns (column, columnWith)
import PostgresqlCodecs.Core (Columns, Params, Report (..), Result, Scalar)
import PostgresqlCodecs.Dimension (Dimension, vector)
import PostgresqlCodecs.Dimensionality (Dimensionality, d0, d1, d2, d3, d4, d5, d6)
import PostgresqlCodecs.Fields (Fields, composite, field, fieldWith)
import PostgresqlCodecs.IsScalar (IsScalar (..))
import PostgresqlCodecs.Nullability (Nullability, nonNullable, nullable)
import PostgresqlCodecs.Params (param, paramWith)
import PostgresqlCodecs.Result (manyRows, oneRow, refine, rowsAffected, someRow)
import PostgresqlCodecs.Scalar (enum)
import PostgresqlTypes
