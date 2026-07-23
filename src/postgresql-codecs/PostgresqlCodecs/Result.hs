module PostgresqlCodecs.Result where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Generic as GenVec
import PostgresqlCodecs.Columns
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Report
import qualified VectorExtras.Accumulator as VecAcc

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

-- | Decode zero or one row; returns 'Nothing' when the result set is empty.
someRow :: Columns row -> Result (Maybe row)
someRow columnsDecoder =
  MaybeRow columnsDecoder (Right Nothing) (Right . Just)

-- | Decode exactly one row; the caller is responsible for ensuring cardinality.
oneRow :: Columns row -> Result row
oneRow columnsDecoder =
  OneRow columnsDecoder Right

-- | Fold all rows into a generic vector.
manyRows :: (GenVec.Vector vec row) => Columns row -> Result (vec row)
manyRows columnsDecoder =
  ManyRows
    columnsDecoder
    VecAcc.init
    (\acc row -> Right (VecAcc.add row acc))
    (\acc -> Right (VecAcc.toVector acc))

-- | Return the number of rows affected by a DML statement.
rowsAffected :: Result Int
rowsAffected =
  RowsAffected Right

-- | Post-compose a refinement onto a result decoder's finalizer.
--
-- A 'Left' produced by the refinement short-circuits the whole result.
refine :: (a -> Either Report b) -> Result a -> Result b
refine f = \case
  RowsAffected toResult -> RowsAffected (\count -> toResult count >>= f)
  MaybeRow columnsDecoder noRow toRow -> MaybeRow columnsDecoder (noRow >>= f) (\row -> toRow row >>= f)
  OneRow columnsDecoder toRow -> OneRow columnsDecoder (\row -> toRow row >>= f)
  ManyRows columnsDecoder initialAcc foldRow finalize -> ManyRows columnsDecoder initialAcc foldRow (\acc -> finalize acc >>= f)

-- | Transitive set of @(schema, name)@ pairs of referenced types that have no
-- statically known base OID and therefore must be resolved at runtime.
unresolvedTypes :: Result a -> Set (Text, Text)
unresolvedTypes = \case
  RowsAffected _ -> Set.empty
  MaybeRow columnsDecoder _ _ -> columnsDecoder.unresolvedTypes
  OneRow columnsDecoder _ -> columnsDecoder.unresolvedTypes
  ManyRows columnsDecoder _ _ _ -> columnsDecoder.unresolvedTypes
