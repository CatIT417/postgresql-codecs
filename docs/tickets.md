# Tickets: Fix the OID bugs from ADR 0001 without adopting its full API redesign

ADR 0001 (`docs/adr/0001-single-value-api.md`, branch `no-result`) proposes replacing
`Result`/`Columns`/`Params`/`ConsumesColumns` with single-value `Decoder`/`Encoder` types.
This work keeps the existing `Scalar → Dimensionality → Nullability → Columns/Params → Result`
hierarchy, but grafts on the ADR's actual bug-fixing mechanism (a driver-supplied `Resolver`
for OIDs the library can't know statically) plus two gaps found while adapting `hasql`:
`Columns` needs per-column expected OIDs, and `Params` needs per-parameter wire format.

Work the **frontier**: any ticket whose blockers are all done. Tickets 2, 5, and 6 can all
start as soon as 1 is done; 3 and 4 wait on 2.

## Thread Resolver through Scalar/Fields/Columns/Params

**What to build:** A new `PostgresqlCodecs.Resolver` module (`TypeOids { baseOid, arrayOid :: Word32 }`,
`0` = unknown; `type Resolver = Text -> Text -> TypeOids`, schema `""` = default schema). `Scalar` and
`Fields` gain `unresolvedTypes :: Set (Text, Text)` (the transitive set of `(schema, name)` pairs with
no static OID) and their binary/textual encoder/decoder fields become `Resolver -> ...` functions —
`standard`/`enum` ignore the resolver, `composite` forwards it into `Fields` and unions in its own
type name. `Columns` gains `unresolvedTypes` and `oids :: Resolver -> [Word32]` (unioned/concatenated
through its `Applicative` instance); its `decoder` becomes `Resolver -> forall f. ConsumesColumns f => f a`.
`Params` gains `unresolvedTypes` and `oids :: Resolver -> [Word32]`; its `binaryEncoder`/`textualEncoder`
become `Resolver -> a -> ...`. This is pure plumbing — no OID guess is fixed yet, the harness passes a
constant `\_ _ -> TypeOids 0 0` resolver everywhere, and every existing integration-test spec passes
unmodified.

**Blocked by:** None — can start immediately

- [ ] `PostgresqlCodecs.Resolver` exists with `TypeOids` and `Resolver`, exported from `PostgresqlCodecs`
- [ ] `Scalar`, `Fields`, `Columns`, `Params` all carry `unresolvedTypes`, correctly unioned/propagated
      through `standard`/`enum`/`composite`/`field`/`column`/`param` and each type's combinator instance
- [ ] `Scalar`'s and `Fields`' four codec fields, `Columns.decoder`, `Columns.oids`, `Params.oids`,
      `Params.binaryEncoder`/`textualEncoder` are all `Resolver`-parameterized
- [ ] The integration-test harness still compiles and passes with a constant no-op resolver
- [ ] `cabal build all` and the full existing integration-test suite pass unchanged

## Real OID catalog resolution + fix binary array-of-enum decode bug

**What to build:** The integration-test harness (`IntegrationTest/Integrations/Pq.hs`) builds a real
`Resolver` before each `execQuery`: union `params.unresolvedTypes` with the executed `Result`'s
`unresolvedTypes`, and if non-empty, run one parameterized query against `pg_type`/`pg_namespace`
(via `to_regtype`, so `""` schema resolves through `search_path` per `Scalar.schemaName`'s documented
meaning) to build a `Map (Text, Text) TypeOids`, then wrap it as a `Resolver` defaulting to
`TypeOids 0 0`; an empty set skips the query entirely. `Columns.column` stops guessing OID `705` for
the array element type and uses the resolver-derived OID instead, fixing binary arrays of
runtime-resolved (enum/composite) scalars, which today always fail with a base-OID mismatch.

**Blocked by:** Thread Resolver through Scalar/Fields/Columns/Params

- [ ] Harness builds a real `Resolver` from a live `pg_type`/`pg_namespace` lookup, skipping the query
      when `unresolvedTypes` is empty
- [ ] `Columns.column`'s binary decoder passes the resolver-derived element OID to
      `Dimensionality.binaryDecoder` instead of `fromMaybe 705`
- [ ] `ScalarSpec`'s local ad-hoc `"genre"` SQL fixture is renamed so it no longer collides with the
      real `genre` type this ticket's test creates
- [ ] New `DimensionalitySpec` test: binary `d1` array round-trip of `Demo.Types.Genre.genre` against a
      live container — demonstrated to fail on `master` and pass here

## Fix composite-field bogus-OID encode bug

**What to build:** `Fields.field` stops writing OID `705` as the field OID in the composite binary
payload (and stops handing `705` to the nested `Dimensionality.binaryDecoder`) for fields whose scalar
has no static OID, using the resolver instead. Fixes composites containing an enum or nested composite
field, which today always encode a bogus field OID onto the wire.

**Blocked by:** Real OID catalog resolution + fix binary array-of-enum decode bug

- [ ] `Fields.field`'s binary encoder and decoder derive the real field OID from the resolver instead
      of `fromMaybe 705`
- [ ] New `FieldsSpec` test: binary round-trip of `Demo.Types.Artist` (a composite with an enum field)
      against a live container — demonstrated to fail on `master` and pass here

## Fix parameter-OID bug

**What to build:** `Params.param` stops collapsing an unresolved OID to `0` (which silently discards
`schemaName`/`typeName` and tells the server to infer the type) and uses the resolver instead, so
enum/composite parameters get a real declared OID whenever the driver can resolve one.

**Blocked by:** Real OID catalog resolution + fix binary array-of-enum decode bug

- [ ] `Params.param`'s `oids` field derives the parameter OID from the resolver instead of `fromMaybe 0`
- [ ] New test asserting the resolved parameter OID for an enum/composite scalar matches the real
      `pg_type.oid` from the live container, rather than `0`

## Result refinement layer

**What to build:** `MaybeRow` and `OneRow` gain `Either Report`-producing finalizers, matching the
shape `ManyRows` already has (its "no row" value becomes `Either Report a` too, so every
finalizer-shaped value in `Result` is uniformly `Either Report`-typed). A new
`refine :: (a -> Either Report b) -> Result a -> Result b` composes cleanly through all four variants
by post-composing each one's finalizer. A small `unresolvedTypes :: Result a -> Set (Text, Text)`
helper extracts the embedded `Columns`' set regardless of which variant is used.

**Blocked by:** Thread Resolver through Scalar/Fields/Columns/Params

- [ ] `RowsAffected`/`MaybeRow`/`OneRow`/`ManyRows` all carry an `Either Report`-producing finalizer
- [ ] `refine` is implemented once, generically, over all four constructors
- [ ] `Result.unresolvedTypes` correctly extracts the set from whichever variant is used
- [ ] New `ResultSpec` tests: a `refine` that succeeds and one that fails, on at least `oneRow` and
      `someRow`

## Per-parameter wire format

**What to build:** `Params` gains `formats :: [Format]` where `data Format = Binary | Textual`,
populated per-parameter from `scalar.supportsBinary` and combined via `<>`/`mempty` alongside `oids`.
The harness computes both the binary and textual encodings for a call and picks per-parameter via
`effectiveFormat = if not isBinaryEncodingAllowed then Textual else declaredFormat` — the global
binary/text test toggle can only downgrade a parameter to text, never force binary onto one that
declared it can't. This makes mixed-format parameter lists expressible, matching what `hasql`'s
`Encoders.unknown` needs (some parameters always textual, others binary, in the same call).

**Blocked by:** Thread Resolver through Scalar/Fields/Columns/Params

- [ ] `Params` carries `formats :: [Format]`, correctly concatenated through its `Semigroup`/`Monoid`
      instance and populated by `param` from `scalar.supportsBinary`
- [ ] Harness selects wire format per-parameter using `effectiveFormat`, computing both encodings per
      call
- [ ] New test scalar with `supportsBinary = False`, and a regression test proving it stays textual
      even in a binary-mode query run alongside a normal binary-capable parameter
