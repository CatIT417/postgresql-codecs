module PostgresqlCodecs.Dimensionality.Writes where

import PostgresqlCodecs.Prelude
import PtrPoker.Write

-- | Encode the binary array header.
-- Arguments: number of dimensions, whether any element may be NULL, element base OID, and per-dimension sizes.
-- Each dimension entry is written as (size, lowerBound=1) per the PostgreSQL binary protocol.
arrayHeader :: Int32 -> Bool -> Word32 -> [Int32] -> Write
arrayHeader dimensionCount elementNullable baseOid dimensions =
  mconcat
    [ bInt32 dimensionCount,
      bInt32 (if elementNullable then 1 else 0),
      bWord32 baseOid,
      foldMap arrayHeaderDimension dimensions
    ]

-- | Encode a single binary array dimension entry as (size, lowerBound=1).
-- The lower bound is always 1, following the PostgreSQL convention for 1-based arrays.
arrayHeaderDimension :: Int32 -> Write
arrayHeaderDimension size =
  bInt32 size <> bInt32 1
