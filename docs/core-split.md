# Splitting the core (driver-facing) API out

Design note, 2026-07-27. Iteration 1 - the internal sublibrary below - shipped the same day; iteration 2 (extracting a separate package) remains deferred.

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
`TypeOids (..)`, `Format (..)`, `Resolver`, `Report (..)`.

The records are the anatomy. `ConsumesColumns`, `Resolver`, `TypeOids`, and `Format` are the
vocabulary that anatomy is written in — `Columns.decoder :: Resolver -> forall f. ConsumesColumns f
=> f a` puts the class literally in the shape of the type.

Nothing that *builds* a `Scalar` lives here. Raw authorship is the `Scalar` constructor itself; there
is no helper wrapping it (see `custom`, below).

Core therefore depends on `ptr-peeker`, `ptr-poker`, `attoparsec`, `text-builder`, `containers`, and
`text` — and notably **not** on `postgresql-types` or `postgresql-types-algebra`. It has no opinion
about which PostgreSQL types exist.

### `PostgresqlCodecs`

The combinators — `param`, `column`, `field`, `d0`–`d6`, `nullable`, `oneRow`, `refine`, `enum`,
`composite` — plus `IsScalar (..)`, `Nullability`, `Dimensionality`, `Dimension`, and `Fields`, which
are constructs of the building language rather than parts of a codec's anatomy: none of them survives
into the value a driver receives.

The core types are re-exported here **abstractly** (`Params`, `Result`, `Columns`, `Scalar` — no
`(..)`), so app-dev type signatures work without reaching for core.

`binaryPrimitive` and `textualPrimitive` also live here, but stay **unexported**. They derive a
`Scalar` from a `postgresql-types-algebra` `IsPrimitive`/`IsBinaryPrimitive` instance — converting
`DecodingError` to `Report`, extracting OIDs, deriving `unresolvedTypes` from missing static ones.
That is derivation from a description, structurally the same move as `enum` and `composite`, so it is
language rather than anatomy. They are kept internal because their only proven caller is this
library's own `IsScalar` instances over the `postgresql-types` types. Exporting them later is a minor
bump; un-exporting is a major break, so hidden is the recoverable default.

The consequence, stated as a rule rather than left as an accident: **`enum` and `composite` are the
only public ways to obtain a `Scalar`.** Anything else means dropping to core and writing
`Scalar {..}`, which is an honest signal about what you are doing.

### Three notes on the edges

**`Report` is off-axis, not an exception.** It gets `(..)` on *both* sides. It is neither anatomy nor
combinator — it is the failure type threaded orthogonally through both halves: drivers receive
`Either Report a` from `consumeColumn`, and app devs *construct* reports in
`refine :: (a -> Either Report b) -> Result a -> Result b`. Hiding its fields app-side would restrict
them to message-only errors via the `IsString` instance, throwing away `suggestion` and `details` — a
real usability cost for no gain. `Report` is also the most frozen type in the library (three
text-ish fields, documented as opaque *for reading*, no evolution pressure), unlike `Scalar`, which
grows whenever a wire format does.

**`custom` is deleted, not moved.** It took nine positional arguments that were exactly the nine
fields of `Scalar` and packed them into the record — zero semantics. With `Scalar (..)` exported from
core it is strictly worse than writing `Scalar {..}`, which names its fields at the call site. It is
not language (nothing is derived or composed) and not anatomy (the constructor already is that). Its
haddock's "escape hatch" role passes to the `Scalar` constructor.

**`Resolver` has no constructors to hide.** It is a type synonym,
`type Resolver = Text -> Text -> TypeOids`; drivers build one as a plain lambda. The core/app
distinction for that pair is carried entirely by `TypeOids (..)`.

## Iteration 1: an internal sublibrary, not a plain module

> **Status: shipped (2026-07-27).** The subsection [What landed (2026-07-27)](#what-landed-2026-07-27)
> at the end records the outcome and the few items deferred.

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

`Scalar.hs` splits the same way and for the same reason: `data Scalar` to core, `binaryPrimitive` /
`textualPrimitive` / `IsScalar` and its instances to the app side. That is what removes the
`postgresql-types` dependency from core.

### What landed (2026-07-27)

Shipped as described above; verified against the `.cabal` and source tree:

- `library core` stanza with a single exposed module `PostgresqlCodecs.Core` and internal
  `Core.{Columns,Params,Prelude,Report,Resolver,Result,Scalar}`. Core depends on `attoparsec`,
  `containers`, `invariant`, `ptr-peeker`, `ptr-poker`, `tagged`, `text`, and `text-builder` - and, as
  intended, **not** on `postgresql-types` or `postgresql-types-algebra`.
- The main library lists `reexported-modules: PostgresqlCodecs.Core`, depends on
  `postgresql-codecs:core`, and re-exports `Params`, `Columns`, `Result`, `Scalar` **abstractly**
  (no `(..)`). `Report (..)` keeps its constructors on both sides. `custom` is gone;
  `binaryPrimitive`/`textualPrimitive` live in the internal `PostgresqlCodecs.Scalar` and stay
  unexported. `enum` and `composite` are the only public ways to obtain a `Scalar`.
- `Params`, `Columns`, and `Scalar` each split type-plus-instances (core) from smart constructors
  (app side), as planned; the cut is acyclic.
- `integration-test` gained an explicit `postgresql-codecs:core` dep, so it consumes the driver API
  the way a real driver would. `demo` did not, because it never reaches for driver anatomy - which
  matches the "where they reach for driver types" qualifier.
- The driver contract haddock moved onto `PostgresqlCodecs.Core`, and the `IsScalar` class haddock now
  points instance authors at `enum`/`composite` or the core `Scalar` constructor.

Two nits deliberately left:

- The `custom` "escape hatch" framing was **not** re-homed onto the core `Scalar` constructor's
  haddock (which is plain). The constructor does the job; the prose just does not say so.
- The blanket `OPTIONS_GHC -Wno-*` pragma still rides on the four core modules that had it before the
  split (`Columns`, `Params`, `Resolver`, `Scalar`) - the hygiene item already flagged below as out of
  scope.

Still pending (see "Also in scope" below): `AGENTS.md`'s `Scalar`-layer description and per-file
project-structure tree still name `custom` / `binaryPrimitive` / `textualPrimitive` and the pre-split
layout. Iteration 2 is unchanged.

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

Status after iteration 1: the two haddock moves shipped; the `AGENTS.md` rewrites did not, and the
`Resolver.hs` hygiene item remains (and now applies to four core modules).

- **[shipped]** Move the ~70-line resolver/driver contract haddock from `PostgresqlCodecs` into
  `PostgresqlCodecs.Core`. `AGENTS.md` points at it as authoritative and will need updating.
- **[shipped]** Fix the `IsScalar` class haddock (`IsScalar.hs`, currently lines 29–32). It tells instance
  authors to define instances in terms of `custom`, `enum`, `composite`, `binaryPrimitive`, or
  `textualPrimitive` — three of which this plan deletes or hides. It is the one place a user is told
  what to write on the right-hand side, so it goes stale the moment the split lands. New wording:
  `enum` or `composite`, or the `Scalar` constructor from core for raw authorship.
- **[pending, minor]** The `custom` haddock's "escape hatch" framing moves onto the `Scalar` constructor in core. Note
  this is separate from the `columnWith`/`paramWith`/`fieldWith` escape hatch, which is about
  bypassing `IsScalar` and is unaffected.
- **[pending]** `AGENTS.md` also describes the `Scalar` layer as "built via `binaryPrimitive`/`textualPrimitive`
  …, `enum`, `composite`, or `custom`" and lists per-file module contents — both go stale here.
- **[out of scope]** `Resolver.hs` carries a blanket `OPTIONS_GHC -Wno-*` pragma suppressing about ten warning classes.
  If it becomes part of a published core API, that should go. Hygiene, not part of this design.
