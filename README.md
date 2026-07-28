# postgresql-codecs

[![Hackage](https://img.shields.io/hackage/v/postgresql-codecs.svg)](https://hackage.haskell.org/package/postgresql-codecs)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/postgresql-codecs/)

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

`column`, `param`, and `field` resolve a type's codec from its `IsScalar` instance — they do not
take a `Scalar` value directly. Every standard PostgreSQL type, and several plain Haskell types
(`Int32`, `Int64`, `Text`, `Bool`, `Double`, `Day`, `LocalTime`, `UTCTime`, `UUID`, `ByteString`,
`Scientific`, …), already has one. Write your own for an enum or composite:

```haskell
import Data.Profunctor (lmap)
import PostgresqlCodecs qualified

-- An enum scalar. The schema and type name must match the server-side `CREATE TYPE`.
-- `enum` requires an `Ord` constraint on the value type.
data Color = Red | Green | Blue
  deriving (Eq, Ord, Show)

instance PostgresqlCodecs.IsScalar Color where
  scalar =
    PostgresqlCodecs.enum "public" "color"
      [ ("red", Red),
        ("green", Green),
        ("blue", Blue)
      ]

-- A composite scalar. Each field is `field <dimensionality> <nullability>`; select the
-- input record field with `lmap`, since `Fields` is a `Profunctor`.
data Point = Point {x :: Int32, y :: Int32}

instance PostgresqlCodecs.IsScalar Point where
  scalar =
    PostgresqlCodecs.composite "public" "point" $
      Point
        <$> lmap (.x) (PostgresqlCodecs.field PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
        <*> lmap (.y) (PostgresqlCodecs.field PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)

-- A query-parameter encoder and a result-column decoder both reuse the instance:
pointParam :: PostgresqlCodecs.Params Point
pointParam = PostgresqlCodecs.param PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable

pointColumn :: PostgresqlCodecs.Columns Point
pointColumn = PostgresqlCodecs.column PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
```

Dimensionality (`d0` for a scalar, `d1`–`d6` for an N-dimensional array over some
`Dimension` like `vector`) and nullability (`nonNullable` / `nullable`) are chosen at each call
site — there is, deliberately, no `IsScalar` instance for `Maybe` or an array/vector type. To
decode several columns into one row, combine `Columns` values applicatively
(`Functor` / `Applicative`); to encode several parameters, combine `Params` values with their
`Semigroup` / `Monoid` instances (`Contravariant`).

A type that needs more than one PostgreSQL representation should not share one Haskell type across
both — a class instance is a single, global, coherence-checked choice per type, so it can't express
that. Define a distinct newtype per representation, each with its own instance. The
`columnWith` / `paramWith` / `fieldWith` trio takes an explicit `Scalar` instead of an instance,
but it exists only for a type with no static representation to hang an instance on (e.g. a codec
resolved at runtime from schema introspection) — never as a bypass for a type that already has, or
should have, an instance.

## For driver authors

This package serves two audiences through two modules. Application code imports
`PostgresqlCodecs` and never sees driver detail. Driver authors import `PostgresqlCodecs.Core`
instead. It is the codec *anatomy* (the record fields of `Params`, `Columns`, `Result`, `Scalar`,
plus `ConsumesColumns`, `Resolver`, `TypeOids`, and `Format`) and documents the OID-resolution
contract a driver must fulfill.
