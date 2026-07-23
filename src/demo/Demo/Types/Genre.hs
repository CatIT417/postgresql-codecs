module Demo.Types.Genre where

import qualified PostgresqlCodecs
import Prelude hiding (Int8, Text)

data Genre = Rock | Pop | Jazz
  deriving (Eq, Ord, Show)

genre :: PostgresqlCodecs.Scalar Genre
genre =
  PostgresqlCodecs.enum
    ""
    "genre"
    [ ("rock", Rock),
      ("pop", Pop),
      ("jazz", Jazz)
    ]
