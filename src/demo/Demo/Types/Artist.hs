module Demo.Types.Artist where

import Demo.Types.Genre
import PostgresqlCodecs
import Prelude hiding (Int8, Text)

data Artist = Artist
  { name :: Text,
    genre :: Genre
  }

artist :: Scalar Artist
artist =
  composite
    ""
    "artist"
    ( Artist
        <$> lmap (.name) (field standard d0 nonNullable)
        <*> lmap (.genre) (field genre d0 nonNullable)
    )
