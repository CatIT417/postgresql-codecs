# postgresql-codecs

**postgresql-codecs** is a driver-agnostic Haskell library for encoding and
decoding PostgreSQL values. It covers both the binary and textual wire formats
and ships no I/O itself - driver integration requires only a small
`ConsumesColumns` instance.

## Install

```cabal
build-depends: postgresql-codecs
```

## Architecture

The library is built from five composable layers:

```
Scalar          - type name, OIDs, binary+textual encoder/decoder
   ↓
Dimensionality  - pass-through (d0) or N-dimensional array (d1–d6)
   ↓
Nullability     - nullable (Maybe) or non-nullable elements
   ↓
Columns/Params  - result-column decoder / query-parameter encoder
   ↓
Result          - full result-set strategy (oneRow, someRow, manyRows, rowsAffected)
```

## Quick example

```haskell
import PostgresqlCodecs qualified

-- A native scalar (uses postgresql-types IsScalar instance)
int4Scalar :: PostgresqlCodecs.Scalar PostgresqlTypes.Int4
int4Scalar = PostgresqlCodecs.native

-- An enum scalar
data Color = Red | Green | Blue

colorScalar :: PostgresqlCodecs.Scalar Color
colorScalar = PostgresqlCodecs.enum "public" "color"
  [ ("red",   Red)
  , ("green", Green)
  , ("blue",  Blue)
  ]

-- A composite scalar
data Point = Point { x :: PostgresqlTypes.Int4, y :: PostgresqlTypes.Int4 }

pointScalar :: PostgresqlCodecs.Scalar Point
pointScalar = PostgresqlCodecs.composite "public" "point" $
  Point
    <$> PostgresqlCodecs.field int4Scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable (.x)
    <*> PostgresqlCodecs.field int4Scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable (.y)

-- Encode as a query parameter and decode as a result column
param :: PostgresqlCodecs.Params Point
param = PostgresqlCodecs.param pointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable

col :: PostgresqlCodecs.Columns Point
col = PostgresqlCodecs.column pointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
```
