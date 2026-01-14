module PostgresqlMapping.Core.Result where

import PostgresqlMapping.Core.Columns
import PostgresqlMapping.Core.Prelude

-- | Result decoder.
data Result a
  = SingleRow (Columns a)
  | forall row. Multirow (Columns row) (Vector row -> a)
  | RowsAffected (Int -> a)

oneRow :: Columns row -> Result row
oneRow = error "TODO"

someRow :: Columns row -> Result (Maybe row)
someRow = error "TODO"

manyRows :: Columns row -> Result (Vector row)
manyRows = error "TODO"

rowsAffected :: Result Int
rowsAffected = error "TODO"
