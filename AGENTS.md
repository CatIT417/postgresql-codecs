# Agent Guide for `postgresql-codecs`

## Project Overview

`postgresql-codecs` is a **driver-agnostic PostgreSQL codecs library** written in **Haskell**. It provides composable encoders and decoders for mapping between Haskell types and PostgreSQL's binary and textual wire formats. The library ships no I/O of its own; driver integration requires only a small `ConsumesColumns` instance.

The library is built from five composable layers, described in the `PostgresqlCodecs` module haddock:

1. `Scalar a` - codec for a single PostgreSQL scalar type: schema/type name, optionally-static base/array OIDs, a binary-support flag, and binary + textual encoder/decoder pairs. Built via `binaryPrimitive`/`textualPrimitive` (from a `postgresql-types` `IsBinaryPrimitive`/`IsPrimitive` instance), `enum`, `composite`, or `custom`. `IsScalar` (`PostgresqlCodecs.Scalar`) is an optional class associating a Haskell type with its one canonical `Scalar` - every standard type has an instance already; write one for your own enum/composite types when they have exactly one obvious representation, and keep passing `Scalar` values explicitly (no instance) when a type needs more than one.
2. `Dimensionality scalar vec` - pass-through (`d0`) or N-dimensional array wrapping (`d1`–`d6`), parameterized by a `Dimension` (e.g. `vector`, for any `Data.Vector.Generic.Vector`).
3. `Nullability a b` - `nullable` (wraps in `Maybe`) or `nonNullable` (rejects NULL with a `Report`).
4. `Columns a` / `Params a` - result-set column decoder and query-parameter encoder, built per-column/per-parameter via `column` / `param`.
5. `Result a` - full result-set decoding strategy: `oneRow`, `someRow`, `manyRows`, `rowsAffected`, with a `refine` combinator for post-hoc validation.

A sixth, cross-cutting piece, added to fix a class of OID-guessing bugs: **`Resolver`** (`PostgresqlCodecs.Resolver`). Codecs that reference PostgreSQL types whose OIDs can't be known statically (`enum`, `composite`, and `binaryPrimitive`/`textualPrimitive` scalars like `citext`/`hstore` whose `IsPrimitive` instance has no static OID) advertise those types via `unresolvedTypes :: Set (Text, Text)`. A driver collects the union of `unresolvedTypes` across the `Params` and `Result` it's about to use, resolves it against the server catalogue if non-empty, and passes the resulting `Resolver` into `Columns.decoder`/`oids` and `Params.oids`/`binaryEncoder`/`textualEncoder`. **The full contract is documented in the `PostgresqlCodecs.Core` module haddock (`src/postgresql-codecs-core/PostgresqlCodecs/Core.hs`) - read it before implementing a driver.** Do not re-derive it from source; the haddock is authoritative and this file only summarizes it.

`Fields a b` (composite-type field codecs, built with `field` and consumed by `composite`) is the one layer not in the five-layer list above but is part of the same stack.

Also present: `Report` (`PostgresqlCodecs.Report`) - the library's single structured error type (`message`, `suggestion`, `[(Text, Text)]` details), used everywhere a decode/encode can fail. There is no separate `EncodingError`/`DecodingError` type.

## Project Structure

```
.
├── cabal.project                  # Cabal project config (tests: True - see Build commands)
├── postgresql-codecs.cabal        # Package definition: library core, library, library demo, test-suite integration-test
├── README.md, LICENSE, AGENTS.md
├── docs/
│   └── tickets.md                 # Ticket log for the Resolver work (historical context, not living docs)
│
├── .github/workflows/             # format-and-test.yaml, release.yaml, deploy-docs-to-pages.yaml
│
├── src/
│   ├── postgresql-codecs-core/                # The core sublibrary: driver-facing codec anatomy
│   │   ├── PostgresqlCodecs/Core.hs           # Public facade: driver-contract haddock + re-exports
│   │   └── PostgresqlCodecs/Core/
│   │       ├── Prelude.hs                     # Lean prelude (9 deps: no postgresql-types, vector, uuid, etc.)
│   │       ├── Report.hs                      # Report - the one structured error type
│   │       ├── Resolver.hs                    # TypeOids, Resolver - the OID-resolution contract
│   │       ├── Scalar.hs                      # Scalar type + Invariant instance
│   │       ├── Params.hs                      # Params type + Format + Contravariant/Semigroup/Monoid
│   │       ├── Columns.hs                     # Columns type + ConsumesColumns + Functor/Applicative
│   │       └── Result.hs                      # Result type + unresolvedTypes helper
│   │
│   └── postgresql-codecs/                    # The main library: app-dev combinators
│       ├── PostgresqlCodecs.hs                # Public re-export module; app-dev facade
│       └── PostgresqlCodecs/
│           ├── Prelude.hs                     # Full prelude (includes postgresql-types, vector, etc.)
│           ├── Scalar.hs                      # enum, binaryPrimitive, textualPrimitive, elementOid, declaredOid
│           ├── Fields.hs                      # Fields, field, fieldWith, composite
│           ├── Nullability.hs                 # Nullability, nullable, nonNullable
│           ├── Dimension.hs                   # Dimension, vector
│           ├── Dimensionality.hs              # Dimensionality, d0–d6
│           ├── Dimensionality/Writes.hs       # Binary array-header writes
│           ├── Columns.hs                     # column, columnWith
│           ├── Params.hs                      # param, paramWith
│           ├── Result.hs                      # oneRow, someRow, manyRows, rowsAffected, refine
│           └── IsScalar.hs                    # IsScalar class + standard-type instances
│
│   ├── demo/                                  # library demo - a worked example (enum + composite scalars)
│   │   └── Demo/Types.hs, Demo/Types/Artist.hs, Demo/Types/Genre.hs
│   │
│   └── integration-test/                      # test-suite integration-test (hspec + testcontainers)
│       ├── Spec.hs                            # {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
│       └── IntegrationTest/
│           ├── SpecHook.hs                    # Container/pool lifecycle, one-time schema DDL, runs specs twice
│           ├── Integrations/
│           │   ├── Pq.hs                      # The libpq driver adapter - reference execQuery + Resolver builder
│           │   └── Pq/RowDecoder.hs           # Threads a column-index cursor through ConsumesColumns
│           ├── ColumnsSpec.hs, ParamsSpec.hs, FieldsSpec.hs, ResultSpec.hs,
│           │   ScalarSpec.hs, DimensionSpec.hs, DimensionalitySpec.hs, NullabilitySpec.hs
```

There is no `postgresql-types-algebra-0.1/` directory in this tree. `postgresql-types-algebra` and `postgresql-types` are ordinary version-bounded Hackage dependencies (`^>=0.1` each) declared in `postgresql-codecs.cabal`; `postgresql-types-algebra` supplies the `IsPrimitive`/`IsBinaryPrimitive` class algebra that `binaryPrimitive`/`textualPrimitive` build on, and `postgresql-types` supplies the concrete standard-type instances, which `PostgresqlCodecs` re-exports wholesale (`module PostgresqlTypes`) for convenience. Don't confuse this with this package's own `IsScalar` (`PostgresqlCodecs.Scalar`) - same-sounding but unrelated: `IsPrimitive`/`IsBinaryPrimitive` classify a type as a PostgreSQL primitive; `IsScalar` associates any type (primitive, enum, or composite) with its one canonical `Scalar` codec.

Several files exist in the working tree but are **not tracked in git** - the repo uses a gitignore whitelist (see Development Conventions below) and these are the author's local conveniences: `build.bash`, `notes.md`, `cabal.project.local`, `.vscode/`. Don't assume they're present in a fresh checkout.

## Build and Test Commands

### Standard Cabal workflow

```bash
# Build the library only
cabal build

# Build everything: library, demo library, and the integration-test suite
# (cabal.project sets `tests: True`, so this also compiles the test-suite)
cabal build all

# Generate Haddock documentation
cabal haddock

# Open documentation
cabal haddock --open
```

### Running the integration test suite

There **is** a test suite: `test-suite integration-test` (hspec + `testcontainers`/`testcontainers-postgresql`) under `src/integration-test/`, discovered via `hspec-discover`.

```bash
# Requires Docker: testcontainers pulls and starts a postgres:18 container.
cabal test integration-test

# Focus on specific examples (hspec's --match, forwarded via cabal's test-options):
cabal test integration-test --test-options='--match "<pattern>"'
```

Do **not** attempt to run this suite without Docker available - it will fail to start the container. If you can't run Docker in your environment, say so rather than silently skipping verification.

Test harness shape, if you need to touch it:
- `IntegrationTest.SpecHook` owns the container, a `resource-pool` connection pool, and the **one-time** test schema DDL (enum/composite `CREATE TYPE` statements), created once in `aroundAll` before the pool is handed to the spec tree - so statements need no `IF NOT EXISTS` guards. `hook` then runs the **entire spec tree twice**, `parallel`-ized, once under `describe "Binary format"` and once under `describe "Text format"`, toggling which wire format parameters are sent in.
- `IntegrationTest.Integrations.Pq` is the libpq driver adapter and the reference `ConsumesColumns` implementation. Its `execQuery` is also the reference example of implementing the `Resolver` contract: it unions `Params.unresolvedTypes` with the `Result`'s `unresolvedTypes`, and if non-empty, runs one parameterized query against `pg_type` (via `to_regtype`) to build a `Map (Text, Text) TypeOids`, wrapped as a `Resolver` defaulting to `TypeOids 0 0`. **This resolver is rebuilt from scratch on every `execQuery` call** - deliberately, for harness simplicity. This is explicitly **not** the pattern to copy in a real driver; see the `PostgresqlCodecs` module haddock's caching guidance (cache per connection).
- `IntegrationTest.Integrations.Pq.RowDecoder` threads a column-index cursor through a row so `ConsumesColumns` can consume columns left-to-right.

### Local build script

`build.bash` sources `~/bin/build-cabal-project`, a personal script **not checked into this repo**. It is not usable by an agent without that script present - use `cabal` directly instead (commands above). The default VS Code build task (`.vscode/tasks.json`, itself untracked) also just runs `./build.bash`.

## Code Style Guidelines

### Language extensions

All language extensions are declared **centrally in the `.cabal` file** (`default-extensions` under the `common base` stanza, shared by the library, `demo`, and `integration-test` via `import: base`). There are no per-file `LANGUAGE` pragmas anywhere in `src/` - keep it that way; add new extensions to the common stanza instead.

Notable enabled extensions: `NoImplicitPrelude`, `OverloadedRecordDot` + `NoFieldSelectors`, `StrictData`, `DerivingStrategies`/`DerivingVia`, `LambdaCase`, `BlockArguments`, `TypeApplications`, `RankNTypes`, `TypeFamilies`, `GADTs`.

**Gotcha:** `Columns.decoder :: Resolver -> (forall f. ConsumesColumns f => f a)` is a rank-2 field. `OverloadedRecordDot`/`getField` do not work on a polymorphic field like this - you must access it by record pattern matching (`PostgresqlCodecs.Columns {decoder} <- ...` or `case columns of Columns {decoder} -> ...`), never `columns.decoder`. See `IntegrationTest.Integrations.Pq.decodeRow` for the working pattern.

### Custom Prelude

Every module of the **library** (`src/postgresql-codecs/`) starts with:

```haskell
import PostgresqlCodecs.Prelude
```

`PostgresqlCodecs.Prelude` (itself `NoImplicitPrelude`-exempt, being the prelude) re-exports a curated set of `base` modules plus `profunctors`, `lawful-conversions`, `QuickCheck`, `text-builder`, and common data types (`Text`, `Vector`, `UUID`, `TextBuilder`). It hides list-specific traversal functions in favor of the `Foldable`/`Traversable` versions.

This convention applies **only to the library**. `src/demo/` uses `rerebase` with a small `Prelude hiding (...)`, and `src/integration-test/` imports plain `Prelude` throughout - both are still under the cabal-wide `NoImplicitPrelude` extension, so every module, including test modules, needs an explicit prelude import of some kind, just not necessarily `PostgresqlCodecs.Prelude`.

### GHC warning pragmas

Some of the actively-evolving implementation modules - currently `Columns.hs`, `Params.hs`, `Resolver.hs`, `Scalar.hs` - start with a warning-silencing pragma:

```haskell
{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing
    -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods
    -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations
    -Wno-missing-signatures #-}
```

This is intentional while these modules keep changing. Don't remove it as a drive-by cleanup; remove it only alongside actually tightening up the module (adding signatures, resolving the shadowing, etc.).

### Module naming

- Public API: `PostgresqlCodecs` - the single exposed library module; everything else in the library is `other-modules`.
- Internal library modules: `PostgresqlCodecs.<Concept>`.
- Demo code: `Demo.Types.<Name>`.
- Test modules: `IntegrationTest.<Concept>Spec` (hspec, discovered by name convention), driver adapter code under `IntegrationTest.Integrations.<Driver>`.

### Error type

Encoding and decoding failures use one structured error record, `Report` (`PostgresqlCodecs.Report`), with `message :: Text`, `suggestion :: Text`, and `details :: [(Text, Text)]`. There is no separate `EncodingError`/`DecodingError` pair. `Report` is explicitly documented as "not intended for pattern matching, only for rendering error messages" - treat it as an opaque, renderable failure, not a typed error hierarchy to switch on. Helpers: `toText`, `suggest`, `addDetail`/`addDetailWithKey`, and the `IsReport`/`IsDetail` ports for building `Report`s from your own error types.

## CI / CD and Release Process

### GitHub Actions workflows

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `format-and-test.yaml` | Push to `master`, PRs, or called by `release.yaml` | Format check, cabal file check, docs generation, build on GHC 9.2 and latest (`--qc-max-success=10000`, doctest skipped) |
| `release.yaml` | Push to `supermajor`, `major`, `minor`, `patch` branches | Runs `format-and-test`, then releases to Hackage (tags not prefixed with `v`) |
| `deploy-docs-to-pages.yaml` | Push to `main` or `master`, or manual `workflow_dispatch` | Deploys Haddock to GitHub Pages |

All workflows reuse actions from `nikita-volkov/haskell-hackage-lib-github-actions-workflows@v4`.

### Releasing

Branch-based versioning: pushing to `supermajor` / `major` / `minor` / `patch` triggers a release to Hackage of the corresponding version bump.

## Development Conventions

### Gitignore strategy

The project uses a **whitelist approach** in `.gitignore` - everything is ignored except an explicit allowlist:

```gitignore
/*
!/.gitignore
!/LICENSE
!/README.md
!/CHANGELOG.md
!/AGENTS.md
!/*.cabal
!/cabal.project
!/.github/
!/docs/
!/design/
!/src/
```

If you add a new top-level file or directory that should be tracked, you **must** add it to this whitelist, or `git` will silently ignore it. (`CHANGELOG.md` and `design/` are allowlisted but don't currently exist - that's fine, the whitelist just permits them if created.)

### Design notes

`notes.md` (untracked) contains the author's private design journal. Not documentation; a scratchpad.

`docs/tickets.md` (tracked) is the ticket log for the `Resolver` work that this file documents - useful historical/motivational context for *why* the OID-resolution design looks the way it does, but not living API documentation; don't treat it as a source of current truth about the codebase the way you would module haddock.

### Dependency bounds

The `.cabal` file specifies **tight version bounds** (e.g. `invariant ^>=0.6.4`, `ptr-poker ^>=0.1.2.16`). Follow this convention when adding or upgrading dependencies.

## Security Considerations

- The library performs **direct pointer operations** via `ptr-peeker` and `ptr-poker` for binary encoding/decoding. Bounds are validated before reads: dimension counts, base OIDs, and array lower bounds are checked before element decoding.
- `enum` and `composite` constructors do not sanitize schema or type names before embedding them in `Report` details or in `unresolvedTypes` - this is fine because these names are programmer-supplied constants, not user input. If you write a custom `Resolver`/driver that turns `unresolvedTypes` into a catalogue query, follow the reference `IntegrationTest.Integrations.Pq.buildResolver`'s lead and pass names as bound query parameters, not string-interpolated SQL.
- The textual decoders use `attoparsec`. Be mindful of parser complexity when extending textual formats (composite-field parsing in `Fields.hs` already handles nested quoting/escaping recursively).
- `NoImplicitPrelude` plus a custom prelude means accidental imports of the wrong `Prelude` can introduce subtle behavior differences. Library code should always use `PostgresqlCodecs.Prelude` (see Custom Prelude above for the exceptions in `demo`/`integration-test`).
