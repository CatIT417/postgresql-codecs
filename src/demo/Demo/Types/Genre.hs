module Demo.Types.Genre where

import PostgresqlCodecs
import Prelude hiding (Int8, Text)

data Genre = Rock | Pop | Jazz
  deriving (Eq, Ord)

genre :: Scalar Genre
genre =
  enum
    ""
    "genre"
    [ ("rock", Rock),
      ("pop", Pop),
      ("jazz", Jazz)
    ]
