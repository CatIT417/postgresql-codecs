module PostgresqlCodecs.Result where

import qualified Data.Vector.Generic as GenVec
import PostgresqlCodecs.Columns
import PostgresqlCodecs.Prelude
import qualified VectorExtras.Accumulator as VecAcc

-- | Result decoder.
data Result a
  = RowsAffected (Int -> Either Text a)
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
      (acc -> row -> Either Text acc)
      -- | Finalizer function.
      (acc -> Either Text a)

someRow :: Columns row -> Result (Maybe row)
someRow columnsDecoder =
  MaybeRow columnsDecoder Nothing Just

oneRow :: Columns row -> Result row
oneRow =
  OneRow

manyRows :: (GenVec.Vector vec row) => Columns row -> Result (vec row)
manyRows columnsDecoder =
  ManyRows
    columnsDecoder
    VecAcc.init
    (\acc row -> Right (VecAcc.add row acc))
    (\acc -> Right (VecAcc.toVector acc))

rowsAffected :: Result Int
rowsAffected =
  RowsAffected Right
