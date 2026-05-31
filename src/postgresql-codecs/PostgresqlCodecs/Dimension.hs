module PostgresqlCodecs.Dimension where

import qualified Data.Vector.Generic as Vg
import PostgresqlCodecs.Prelude

-- | A 'Dimension' encodes the dimensionality of an array type, and how to construct and destruct the underlying representation.
--
-- @l@ is the lower dimensionality, and @h@ is the higher dimensionality.
data Dimension l h = Dimension
  { -- | For constructing the dimension representation during decoding.
    construct :: forall m. (Monad m) => Int -> m l -> m h,
    -- | For destructuring the dimension representation during encoding.
    destruct :: forall a. (a -> l -> a) -> (a -> h -> a),
    count :: h -> Int32
  }

-- | A 'Dimension' that uses any 'Vg.Vector' as the underlying representation.
vector :: (Vg.Vector v a) => Dimension a (v a)
vector =
  Dimension
    { construct = Vg.replicateM,
      destruct = Vg.foldl',
      count = fromIntegral . Vg.length
    }
