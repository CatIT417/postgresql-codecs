module PostgresqlCodecs.Result where

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
      a
      -- | When the query returns a single row, this function is used to map it.
      (row -> a)
  | OneRow (Columns a)
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
  MaybeRow columnsDecoder Nothing Just

-- | Decode exactly one row; the caller is responsible for ensuring cardinality.
oneRow :: Columns row -> Result row
oneRow =
  OneRow

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
