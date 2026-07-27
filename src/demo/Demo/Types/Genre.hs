module Demo.Types.Genre where

import qualified PostgresqlCodecs
import Prelude hiding (Int8, Text)

data Genre = Rock | Pop | Jazz
  deriving (Eq, Ord, Show)

instance PostgresqlCodecs.IsScalar Genre where
  scalar =
    PostgresqlCodecs.enum
      ""
      "genre"
      [ ("rock", Rock),
        ("pop", Pop),
        ("jazz", Jazz)
      ]
