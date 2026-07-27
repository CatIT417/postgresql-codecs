module Demo.Types.Artist where

import Demo.Types.Genre
import qualified PostgresqlCodecs
import Prelude hiding (Int8)

data Artist = Artist
  { name :: PostgresqlCodecs.Text,
    genre :: Genre
  }
  deriving stock (Eq, Show)

instance PostgresqlCodecs.IsScalar Artist where
  scalar =
    PostgresqlCodecs.composite
      ""
      "artist"
      ( Artist
          <$> lmap (.name) (PostgresqlCodecs.field PostgresqlCodecs.scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          <*> lmap (.genre) (PostgresqlCodecs.field PostgresqlCodecs.scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
      )
