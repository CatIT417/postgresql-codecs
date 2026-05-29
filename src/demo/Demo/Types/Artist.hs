module Demo.Types.Artist where

import Demo.Types.Genre
import qualified PostgresqlCodecs
import Prelude hiding (Int8)

data Artist = Artist
  { name :: PostgresqlCodecs.Text,
    genre :: Genre
  }

artist :: PostgresqlCodecs.Scalar Artist
artist =
  PostgresqlCodecs.composite
    ""
    "artist"
    ( Artist
        <$> lmap (.name) (PostgresqlCodecs.field PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
        <*> lmap (.genre) (PostgresqlCodecs.field genre PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
    )
