module PostgresqlMapping
  ( -- * Statement mapping
    ParameterizesStatement (..),
    StatementOf (..),

    -- * Scalar mapping
    MapsToScalar (..),

    -- * Errors
    DecodingError (..),
    DecodingErrorReason (..),

    -- * Parameters
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

    -- ** Dimension
    Dimension,
    vector,

    -- * Scalar
    Scalar,
    enum,
    composite,

    -- ** Composite fields
    Fields,
    field,

    -- * Standard types
    module PostgresqlTypes,
  )
where

import PostgresqlCodecs hiding (column, field, param)
import PostgresqlMapping.MapsToScalar
import PostgresqlMapping.ParameterizesStatement
import PostgresqlTypes
