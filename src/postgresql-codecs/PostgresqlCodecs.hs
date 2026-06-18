-- | Driver-agnostic PostgreSQL codec library.
--
-- Provides a five-layer abstraction for encoding and decoding PostgreSQL values:
--
-- 1. 'Scalar' — a single PostgreSQL scalar type with binary\/textual encoders and decoders.
-- 2. 'Dimensionality' — pass-through ('d0') or N-dimensional array wrapping ('d1'–'d6').
-- 3. 'Nullability' — whether values may be NULL ('nullable' or 'nonNullable').
-- 4. 'Columns' \/ 'Params' — result-set column decoder and query parameter encoder.
-- 5. 'Result' — full result-set decoding strategy.
module PostgresqlCodecs
  ( -- * Report
    Report (..),

    -- * Parameter
    Params (..),
    param,

    -- * Result sets
    Result (..),
    oneRow,
    someRow,
    manyRows,
    rowsAffected,

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
    standard,
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
import PostgresqlCodecs.Result
import PostgresqlCodecs.Scalar
import PostgresqlTypes
