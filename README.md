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

Every standard PostgreSQL type already has an `IsScalar` instance, so `scalar` resolves to its
codec from context. Write your own instance for a type with exactly one obvious representation —
an enum or a composite, typically:

```haskell
import PostgresqlCodecs qualified

-- An enum scalar
data Color = Red | Green | Blue

instance PostgresqlCodecs.IsScalar Color where
  scalar = PostgresqlCodecs.enum "public" "color"
    [ ("red",   Red)
    , ("green", Green)
    , ("blue",  Blue)
    ]

-- A composite scalar
data Point = Point { x :: PostgresqlTypes.Int4, y :: PostgresqlTypes.Int4 }

instance PostgresqlCodecs.IsScalar Point where
  scalar = PostgresqlCodecs.composite "public" "point" $
    Point
      <$> PostgresqlCodecs.field PostgresqlCodecs.scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable (.x)
      <*> PostgresqlCodecs.field PostgresqlCodecs.scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable (.y)

-- Encoder as a query parameter and decode as a result column
param :: PostgresqlCodecs.Params Point
param = PostgresqlCodecs.param PostgresqlCodecs.scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable

-- Decoder as a result column
col :: PostgresqlCodecs.Columns Point
col = PostgresqlCodecs.column PostgresqlCodecs.scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
```

If a type needs more than one PostgreSQL representation (e.g. mapped against two different
schemas, or a test-only variant), skip the instance and keep building and naming `Scalar` values
explicitly with `enum`/`composite`/`custom` — a class instance is a single, coherence-checked,
global choice per type, so it can't express that.
