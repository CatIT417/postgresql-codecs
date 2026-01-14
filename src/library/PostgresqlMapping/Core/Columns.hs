{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlMapping.Core.Columns where

import PostgresqlMapping.Core.DecodingError
import PostgresqlMapping.Core.Dimensionality
import PostgresqlMapping.Core.Nullability
import PostgresqlMapping.Core.Prelude
import PostgresqlMapping.Core.Scalar

-- | Result set columns decoder.
data Columns a = Columns
  { count :: Int32,
    binaryDecoder :: [Maybe ByteString] -> Either DecodingError a
  }

instance Functor Columns

instance Applicative Columns

column :: Scalar a -> Dimensionality a b -> Nullability b c -> Columns c
column = error "TODO"
