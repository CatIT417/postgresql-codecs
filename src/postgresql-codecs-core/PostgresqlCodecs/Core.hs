-- | Core, driver-facing API of @postgresql-codecs@: the /anatomy/ of codecs.
--
-- Where "PostgresqlCodecs" is the language for /building/ codecs (combinators
-- like 'PostgresqlCodecs.param', 'PostgresqlCodecs.column', 'PostgresqlCodecs.enum'),
-- this module is what a codec /is/: the record representations a driver
-- deconstructs, and the small vocabulary ('ConsumesColumns', 'Resolver',
-- 'TypeOids', 'Format') those records are written in.
--
-- App developers normally never import this module — "PostgresqlCodecs"
-- re-exports the codec types abstractly (without their constructors) so type
-- signatures like @'Params' a@ and @'Result' a@ compile without reaching for
-- core. /Driver authors/ import this module: they pattern-match on the records,
-- implement a 'ConsumesColumns' instance, and fulfill the OID-resolution
-- contract below.
--
-- = Resolving type OIDs
--
-- Codecs built from 'PostgresqlCodecs.enum' or 'PostgresqlCodecs.composite',
-- and scalars derived from @postgresql-types@ primitives whose @IsPrimitive@
-- instance has no static OID (e.g. @citext@, @hstore@), reference PostgreSQL
-- types whose OIDs this library cannot know statically. Such a codec advertises
-- the types it needs resolved via its @unresolvedTypes :: Set (Text, Text)@
-- field, a set of @(schemaName, typeName)@ pairs. An empty @schemaName@ denotes
-- the default schema, to be resolved through the server's @search_path@. A
-- driver author owes the library the following:
--
-- 1. Before executing a statement, collect the union of the @unresolvedTypes@
--    of everything about to be used — for this library's shape that is
--    @params.unresolvedTypes <> 'unresolvedTypes' result@. An empty set means
--    zero catalogue round-trips, which is the common case for codecs built only
--    from statically-known primitive scalars.
-- 2. If that set is non-empty, look the types up in the server catalogue (e.g.
--    via @to_regtype@) and build ONE 'Resolver' from the result.
-- 3. Pass that single 'Resolver' to every resolver-parameterized field touched
--    for the statement: @'Columns'.decoder@, @'Columns'.oids@, @'Params'.oids@,
--    and @'Params'.binaryEncoder@ \/ @'Params'.textualEncoder@.
-- 4. Understand what an unresolved type costs. 'Resolver' is total: it returns
--    @'TypeOids' 0 0@ for anything it cannot resolve, @0@ being PostgreSQL's
--    \"unspecified\" sentinel. For a scalar slot that degrades gracefully — the
--    server infers the type or reports an error naming it. For a binary array
--    it does not: the library's own array-header check compares the expected
--    element OID against the one the server sends, so an expected @0@ fails the
--    decode with a @Base OID mismatch@ 'Report'. Resolution is therefore
--    mandatory, not best-effort, for a binary array-typed codec.
-- 5. Cache resolutions, ideally per connection (OIDs of user-defined types are
--    stable within a database but differ across databases). The integration
--    test harness in this repository re-queries @pg_type@ on every query for
--    simplicity; that is NOT the pattern to copy for performance.
--
-- The textual wire format embeds no OIDs, so the textual encoders and decoders
-- ignore their 'Resolver' argument; they still take one so drivers can treat
-- both wire formats uniformly. This applies to the codecs only — a textual
-- parameter is still declared to the server by OID, so @'Params'.oids@ needs a
-- resolver regardless of wire format.
module PostgresqlCodecs.Core
  ( -- * Report
    module PostgresqlCodecs.Core.Report,

    -- * Resolver
    module PostgresqlCodecs.Core.Resolver,

    -- * Parameter
    module PostgresqlCodecs.Core.Params,

    -- * Result sets
    module PostgresqlCodecs.Core.Result,

    -- ** Result set columns
    module PostgresqlCodecs.Core.Columns,

    -- * Scalar types
    module PostgresqlCodecs.Core.Scalar,
  )
where

import PostgresqlCodecs.Core.Columns
import PostgresqlCodecs.Core.Params
import PostgresqlCodecs.Core.Report
import PostgresqlCodecs.Core.Resolver
import PostgresqlCodecs.Core.Result
import PostgresqlCodecs.Core.Scalar
