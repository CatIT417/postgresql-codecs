# Splitting the core (driver-facing) API out

Design note, 2026-07-27. Decided but not yet implemented.

## The problem

`postgresql-codecs` serves two audiences through one module:

- **App devs** compose codecs — `param`, `column`, `field`, `d0`–`d6`, `nullable`, `oneRow`, `refine`.
- **Driver authors** deconstruct them — the record fields of `Params`, `Columns`, `Result`, plus
  `ConsumesColumns`, `Resolver`, `TypeOids`.

Today `PostgresqlCodecs` is the single exposed module and exports both, `(..)` and all. Roughly
seventy lines of its haddock are a driver-author contract (resolver protocol, caching, OID-mismatch
semantics) that no app dev needs to read, sitting on the page every app dev reads first.

Three distinct pains, in priority order:

1. **Documentation / discoverability.** The app-dev landing page is dominated by driver-contract
   material.
2. **Bounds churn.** A constructor change is a major bump for drivers and invisible to app devs, but
   PVP majors are per-package, so every app dev edits their upper bound for a release that cannot
   affect them.
3. **Semantic honesty.** The version number should mean something precise to each audience.

Only pain 2 needs separate packages. Pain 1 — the priority — needs only a module boundary.

## The seam

Not "constructors vs. functions", and not "drivers vs. app devs" either — no driver ever touches
`Scalar`, so an audience-based rule is false for a third of core's contents.

The axis is **anatomy vs. language**. Core is *what a codec is*: the representation, and the
vocabulary its fields are written in. The main library is *the language for building one*: the
combinators, and the constructs that exist only during construction and never survive into the value
a driver receives.

The two halves have independent change drivers, which is the real case for eventually giving them
independent version numbers: anatomy churns when the wire protocol, the OID model, or the set of
formats changes; the language churns when ergonomics change.

### `PostgresqlCodecs.Core`

`Params (..)`, `Columns (..)`, `Result (..)`, `Scalar (..)`, `ConsumesColumns (..)`,
`TypeOids (..)`, `Format (..)`, `Resolver`, `Report (..)`, and the raw scalar constructors
`custom`, `binaryPrimitive`, `textualPrimitive`.

The records are the anatomy. `ConsumesColumns`, `Resolver`, `TypeOids`, and `Format` are the
vocabulary that anatomy is written in — `Columns.decoder :: Resolver -> forall f. ConsumesColumns f
=> f a` puts the class literally in the shape of the type. And the three scalar constructors fit
without special pleading: they are the introduction forms that hand back a representation *directly*,
from raw encoder/decoder pairs. Nothing is composed.

`enum` and `composite` are combinators over existing scalars, not raw authorship, so they stay in the
language half. That line also decides where the "vocabulary extender" audience falls, and it falls
mostly app-side, which is what we want.

### `PostgresqlCodecs`

The combinators — `param`, `column`, `field`, `d0`–`d6`, `nullable`, `oneRow`, `refine`, `enum`,
`composite` — plus `IsScalar (..)`, `Nullability`, `Dimensionality`, `Dimension`, and `Fields`, which
are constructs of the building language rather than parts of a codec's anatomy: none of them survives
into the value a driver receives.

The core types are re-exported here **abstractly** (`Params`, `Result`, `Columns`, `Scalar` — no
`(..)`), so app-dev type signatures work without reaching for core.

### Two notes on the edges

**`Report` is off-axis, not an exception.** It gets `(..)` on *both* sides. It is neither anatomy nor
combinator — it is the failure type threaded orthogonally through both halves: drivers receive
`Either Report a` from `consumeColumn`, and app devs *construct* reports in
`refine :: (a -> Either Report b) -> Result a -> Result b`. Hiding its fields app-side would restrict
them to message-only errors via the `IsString` instance, throwing away `suggestion` and `details` — a
real usability cost for no gain. `Report` is also the most frozen type in the library (three
text-ish fields, documented as opaque *for reading*, no evolution pressure), unlike `Scalar`, which
grows whenever a wire format does.

**`Resolver` has no constructors to hide.** It is a type synonym,
`type Resolver = Text -> Text -> TypeOids`; drivers build one as a plain lambda. The core/app
distinction for that pair is carried entirely by `TypeOids (..)`.

## Iteration 1: an internal sublibrary, not a plain module

Core goes into a `library core` stanza in the existing `.cabal`, with the main library depending on
`postgresql-codecs:core`.

Chosen over a plain module because it is strictly better at nearly identical cost. It delivers pain 1
in full (separate haddock, driver contract off the app-dev landing page), and it makes the dependency
direction **compiler-enforced** — a backward import fails the build instead of quietly rotting until
extraction day. Cost is zero Hackage choreography: one package, one version number. `cabal-version`
is already `3.0`.

`demo` and `integration-test` gain explicit `postgresql-codecs:core` deps where they reach for driver
types. That is arguably a feature: it forces the integration test to consume the driver API the way a
real driver would.

### Module naming is final now

Core exposes exactly one module, `PostgresqlCodecs.Core`, matching the repo's established
one-facade convention. This name must not change on extraction — driver authors import it, and
renaming later would be a second breaking change for precisely the audience this exercise protects.
Keeping the facade single also leaves the internal file layout free to move, which matters because
moving it is the first thing we do.

### The one piece of real code motion

`Params.hs` and `Columns.hs` currently import `Dimensionality`, `IsScalar`, and `Nullability` — all
app-side — so the cut is cyclic as the files stand.

This is file layout, not design. The type declarations are clean: `data Params` and `data Columns`
reference only `Resolver`, `Report`, `Format`, and base/containers. The cycle exists solely because
the smart constructors `param` and `column` live in the same files as their types. Split each file —
type and instances to core, `param` / `column` to the app side — and the cut is acyclic.

## Iteration 2: extracting the package

**Trigger:** when `postgresql-codecs` starts being promoted as a library for defining codecs. That is
the moment pain 2 becomes real — bounds churn is priced per dependent, and until there is an
audience it is free.

**Timing: the release *before* the promotion push, not the release that is the promotion push.**
Because the module name was frozen in iteration 1, extraction is not a code change for anyone:
`PostgresqlCodecs` still exists and still re-exports abstractly, `PostgresqlCodecs.Core` keeps its
name, and driver authors edit one `build-depends` line. That is cheap — which is exactly why it
should be spent while the audience is one person, rather than in the release newcomers are being
asked to adopt.

Three things to stay honest about:

- The split hardens the *settled* half. Core has already been shaken out by a real libpq integration
  test; the composition API is the one still taking feedback. Splitting does nothing for the surface
  most likely to move at promotion time.
- Packaging does not confer stability. If promotion requires a stable API, the deliverable is a
  commitment to freeze the app surface for some number of releases — enforceable in a single package
  just as well.
- It may never fire, and that is a success rather than a failure to follow through. The sublibrary
  captures nearly all the value at nearly none of the cost.

## Also in scope

- Move the ~70-line resolver/driver contract haddock from `PostgresqlCodecs` into
  `PostgresqlCodecs.Core`. `AGENTS.md` points at it as authoritative and will need updating.
- `Resolver.hs` carries a blanket `OPTIONS_GHC -Wno-*` pragma suppressing about ten warning classes.
  If it becomes part of a published core API, that should go. Hygiene, not part of this design.
