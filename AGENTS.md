# Agent Guide for `postgresql-codecs`

## Project Overview

`postgresql-codecs` is a **driver-agnostic PostgreSQL codecs library** written in **Haskell**. It provides composable encoders and decoders for mapping between Haskell types and PostgreSQL's binary and textual wire formats.

The library is structured in layers:

1. **`postgresql-types-algebra`** (sub-package under `postgresql-types-algebra-0.1/`) — Defines the core `IsScalar` type class algebra, decoding/encoding error types, and range/multirange evidence classes. This is a lightweight, separate package meant to be depended on by libraries that define their own PostgreSQL type mappings.
2. **`postgresql-codecs`** (main package) — Builds on top of the algebra and the `postgresql-types` ecosystem to provide:
   - `Scalar a` — Codecs for individual PostgreSQL scalar types (binary + textual encode/decode).
   - `Columns a` — Result-set column decoders.
   - `Params a` — Query parameter encoders.
   - `Result a` — High-level result decoders (rows affected, zero-or-one, exactly-one, many rows).
   - `Fields a b` — Composite type field codecs (used with `composite` to build custom `Scalar`s).
   - `Nullability a b` — Nullable / non-nullable wrappers.
   - `Dimensionality scalar vec` — Array dimensionality combinators (`d0`, `d1`, `d2`).

The library does **not** talk to PostgreSQL directly; it produces bytes and parsers that can be plugged into any PostgreSQL driver.

## Project Structure

```
.
├── cabal.project                          # Cabal project configuration
├── cabal.project.local                    # Local compiler override (optional, usually commented out)
├── postgresql-codecs.cabal                # Main package definition
├── build.bash                             # Local build script (sourced from ~/bin/build-cabal-project)
├── notes.md                               # Design notes and TODOs for the author
│
├── postgresql-types-algebra-0.1/
│   ├── postgresql-types-algebra.cabal     # Sub-package: core algebra
│   └── src/library/
│       └── PostgresqlTypes/Algebra.hs     # IsScalar, IsRangeElement, DecodingError, etc.
│
└── src/
    ├── postgresql-codecs/
    │   ├── PostgresqlCodecs.hs            # Main re-export module
    │   ├── PostgresqlCodecs/Prelude.hs    # Custom prelude (NoImplicitPrelude)
    │       ├── PostgresqlCodecs/Scalar.hs     # Scalar type + standard/enum/composite constructors
    │   ├── PostgresqlCodecs/Columns.hs    # Columns result decoder + driver port type classes
    │   ├── PostgresqlCodecs/Params.hs     # Params encoder (partially TODO)
    │   ├── PostgresqlCodecs/Result.hs     # Result cardinality combinators
    │   ├── PostgresqlCodecs/Fields.hs     # Composite field codecs
    │   ├── PostgresqlCodecs/Nullability.hs# nullable / nonNullable
    │   ├── PostgresqlCodecs/Dimensionality.hs # Array dimension combinators (d0, d1, d2)
    │   ├── PostgresqlCodecs/Dimension.hs  # Dimension abstraction (e.g., vector)
    │   ├── PostgresqlCodecs/Dimensionality/Writes.hs # Array header binary writes
    │   ├── PostgresqlCodecs/DecodingError.hs
    │   └── PostgresqlCodecs/EncodingError.hs
    │
    └── demo/
        ├── Demo/Types.hs                  # Demo re-export module
        ├── Demo/Types/Genre.hs            # Example enum scalar
        └── Demo/Types/Artist.hs           # Example composite scalar
```

## Build and Test Commands

### Standard Cabal workflow

```bash
# Configure and build the project
cabal build

# Build the demo library too
cabal build all

# Generate Haddock documentation
cabal haddock

# Open documentation
cabal haddock --open
```

### Local build script

The repo includes a `build.bash` script that sources `~/bin/build-cabal-project` (a personal script not checked into the repo). The script currently enables:

- `preformat=true` — Code formatting (likely `ormolu` or `fourmolu`)
- `lint=true` — Linting (likely `hlint`)
- `compile=true` — Compilation
- `doc=true` — Documentation generation

If you have the author's `build-cabal-project` script installed, simply run:

```bash
./build.bash
```

### VS Code integration

A `.vscode/tasks.json` is present. The default build task (`Cmd+Shift+B` on macOS / `Ctrl+Shift+B` on Linux/Windows) runs `./build.bash`.

### Testing

**There is no test suite stanza in the `.cabal` file yet.** The project is under active development and several modules contain `error "TODO"` placeholders (e.g., textual array encoding/decoding, 2D arrays, `Params.param`).

CI still passes `--qc-max-success=10000` to the test runner, implying that tests may be added via doctest or a future test-suite. When adding tests, use `tasty` + `tasty-quickcheck` or `tasty-hunit` to match the author's typical Hackage workflow.

## Code Style Guidelines

### Language extensions

All language extensions are declared **centrally in the `.cabal` file** (`default-extensions` under the `common base` stanza). Do not add `LANGUAGE` pragmas to individual files unless it is truly file-specific.

Notable enabled extensions include:

- `NoImplicitPrelude` — Always import `PostgresqlCodecs.Prelude` instead of `Prelude`.
- `OverloadedRecordDot`, `NoFieldSelectors` — Use record dot syntax (`scalar.baseOid`).
- `StrictData` — Record fields are strict by default.
- `DerivingStrategies`, `DerivingVia` — Explicit derivation strategies.
- `LambdaCase`, `BlockArguments`, `TypeApplications` — Modern syntax.
- `RankNTypes`, `TypeFamilies`, `GADTs` — Advanced types used in the public API.

### Custom Prelude

Every module in the library should start with:

```haskell
import PostgresqlCodecs.Prelude
```

The custom prelude re-exports a curated set of base modules, `profunctors`, `invariant`, `lawful-conversions`, `QuickCheck`, `text-builder`, and common data types (`Text`, `Vector`, `UUID`, etc.). It explicitly hides list-traversal functions in favor of `Foldable`/`Traversable` ones.

### GHC warning pragmas

Many implementation modules start with a large `OPTIONS_GHC` pragma that silences common warnings during active development:

```haskell
{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing
    -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods
    -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations
    -Wno-missing-signatures #-}
```

This is intentional while the API is still stabilizing. Do **not** remove these pragmas unless you are also filling in the missing implementations and adding type signatures.

### Module naming

- Public API: `PostgresqlCodecs` (single re-export module).
- Internal modules: `PostgresqlCodecs.<Concept>`.
- Demo code: `Demo.Types.<Name>`.

### Error types

Encoding and decoding failures use structured error records (`EncodingError` and `DecodingError`) rather than plain `String` or `Text`. When propagating errors, populate all relevant fields (`message`, `details`, `inputLocation`, `failingInput`, `expectedInputs`, etc.).

## Testing Instructions

As noted above, there is currently no executable test suite. To verify correctness:

1. **Build everything**:
   ```bash
   cabal build all
   ```

2. **Check that the demo library compiles** — it exercises `enum` and `composite` usage:
   ```bash
   cabal build demo
   ```

3. **Run Haddock** to ensure documentation is valid:
   ```bash
   cabal haddock
   ```

4. **When adding tests**, create a `test-suite` stanza in `postgresql-codecs.cabal` and place test modules under a new `test/` directory. The CI reusable workflow expects tests to accept `--qc-max-success=N`.

## CI / CD and Release Process

### GitHub Actions workflows

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `format-and-test.yaml` | Push to `master`, PRs | Format check, cabal file check, docs generation, build on GHC 9.2 and latest |
| `release.yaml` | Push to `supermajor`, `major`, `minor`, `patch` branches | Runs format-and-test, then releases to Hackage |
| `deploy-docs-to-pages.yaml` | Push to `master`, manual | Deploys Haddock to GitHub Pages |

All workflows reuse actions from `nikita-volkov/haskell-hackage-lib-github-actions-workflows@v4`.

### Releasing

The project uses **branch-based versioning**:

- `supermajor` — Major API breakage
- `major` — Significant feature release
- `minor` — Backward-compatible additions
- `patch` — Bug fixes

Pushing to any of these branches triggers the release workflow, which uploads the package to Hackage and tags the commit (tags are **not** prefixed with `v`).

## Development Conventions

### Gitignore strategy

The project uses a **whitelist approach** in `.gitignore`:

```gitignore
/*
!/.gitignore
!/LICENSE
!/README.md
!/CHANGELOG.md
!/*.cabal
!/cabal.project
!/.github/
!/design/
!/src/
```

Only explicitly listed files and directories are tracked. If you add a new top-level file or directory (e.g., `test/`), you **must** add it to `.gitignore`.

### Design notes

`notes.md` contains the author's private design journal (renaming ideas, layering decisions, statement-mapping DSL experiments). It is not documentation; treat it as a scratchpad.

### Dependency bounds

The `.cabal` file specifies **tight version bounds** (e.g., `invariant ^>=0.6.4`, `ptr-poker ^>=0.1.2.16`). When adding or upgrading dependencies, follow this convention and test with both the minimum and maximum allowed versions.

### Work-in-progress markers

Several parts of the codebase are incomplete and marked with `error "TODO"`:

- `Params.param` — Parameter encoding is not yet implemented.
- `Dimensionality.d2` — 2D array support is not yet implemented.
- Textual encoding/decoding for arrays and composites — Several `error "TODO"` placeholders in `Dimensionality`, `Fields`, and `Scalar`.

When working on these, replace the `error` with a real implementation and add corresponding tests.

## Security Considerations

- The library performs **direct pointer operations** via `ptr-peeker` and `ptr-poker` for binary encoding/decoding. Ensure bounds are respected; the array decoders already validate dimension counts, base OIDs, and lower bounds before reading elements.
- `enum` and `composite` constructors do not sanitize schema or type names before embedding them in error messages — this is fine because they are programmer-supplied constants, not user input.
- The textual decoders use `attoparsec`. Be mindful of parser complexity when adding new textual formats (e.g., arrays in textual format could be recursive).
- `NoImplicitPrelude` plus a custom prelude means that accidental imports of the wrong `Prelude` can introduce subtle behavior differences. Always use `PostgresqlCodecs.Prelude` in library code.
