module PostgresqlCodecs.Result where

import qualified Data.Vector.Generic as GenVec
import PostgresqlCodecs.Core (Columns, Report, Result (..))
import PostgresqlCodecs.Prelude
import qualified VectorExtras.Accumulator as VecAcc

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
