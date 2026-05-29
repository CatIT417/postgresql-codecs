module PostgresqlCodecs
  ( -- * Errors
    DecodingError (..),
    EncodingError (..),

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

    -- ** Dimension
    Dimension,
    vector,

    -- * Scalar types
    Scalar,
    native,
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
import PostgresqlCodecs.DecodingError
import PostgresqlCodecs.Dimension
import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.EncodingError
import PostgresqlCodecs.Fields
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Params
import PostgresqlCodecs.Result
import PostgresqlCodecs.Scalar
import PostgresqlTypes
