module PostgresqlMapping.Core.Dimension where

import qualified Data.Vector as Vector
import PostgresqlMapping.Core.Prelude

data Dimension scalar vec = Dimension
  { -- | For constructing the dimension representation during decoding.
    construct :: forall m. (Monad m) => Int -> m scalar -> m vec,
    -- | For destructuring the dimension representation during encoding.
    destruct :: forall a. (a -> scalar -> a) -> (a -> vec -> a),
    count :: vec -> Int32
  }

vector :: Dimension a (Vector a)
vector =
  Dimension
    { construct = Vector.replicateM,
      destruct = Vector.foldl',
      count = fromIntegral . Vector.length
    }
