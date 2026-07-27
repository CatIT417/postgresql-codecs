module PostgresqlCodecs.Core.Result where

import Data.Set (Set)
import qualified Data.Set as Set
import PostgresqlCodecs.Core.Columns
import PostgresqlCodecs.Core.Prelude
import PostgresqlCodecs.Core.Report

-- | Strategy for consuming a full PostgreSQL result set.
data Result a
  = RowsAffected (Int -> Either Report a)
  | forall row. MaybeRow
      (Columns row)
      -- | When the query returns no rows, this value is returned.
      (Either Report a)
      -- | When the query returns a single row, this function is used to map it.
      (row -> Either Report a)
  | forall row. OneRow
      (Columns row)
      -- | Finalizer applied to the decoded row.
      (row -> Either Report a)
  | forall row acc. ManyRows
      -- | Row decoder.
      (Columns row)
      -- | Initial accumulator value.
      acc
      -- | Row folding function.
      (acc -> row -> Either Report acc)
      -- | Finalizer function.
      (acc -> Either Report a)

-- | Transitive set of @(schema, name)@ pairs of referenced types whose base
-- or array OID is not statically known and therefore must be resolved at runtime.
unresolvedTypes :: Result a -> Set (Text, Text)
unresolvedTypes = \case
  RowsAffected _ -> Set.empty
  MaybeRow columnsDecoder _ _ -> columnsDecoder.unresolvedTypes
  OneRow columnsDecoder _ -> columnsDecoder.unresolvedTypes
  ManyRows columnsDecoder _ _ _ -> columnsDecoder.unresolvedTypes
