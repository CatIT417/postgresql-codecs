module PostgresqlMapping.Core
  ( -- * Errors
    DecodingError (..),
    DecodingErrorReason (..),

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
    primitive,
    enum,
    composite,

    -- * Composite types
    Fields,
    field,
  )
where

import PostgresqlMapping.Core.Columns
import PostgresqlMapping.Core.DecodingError
import PostgresqlMapping.Core.Dimension
import PostgresqlMapping.Core.Dimensionality
import PostgresqlMapping.Core.Fields
import PostgresqlMapping.Core.Nullability
import PostgresqlMapping.Core.Params
import PostgresqlMapping.Core.Result
import PostgresqlMapping.Core.Scalar
