{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Params where

import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.EncodingError
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Scalar
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Parameterized query parameters encoder.
data Params a = Params
  { count :: Int32,
    binaryEncoder :: [a -> Either EncodingError (Maybe Write.Write)],
    textualEncoder :: [a -> TextBuilder.TextBuilder]
  }

instance Contravariant Params

instance Semigroup (Params a)

instance Monoid (Params a)

param :: Scalar a -> Dimensionality a b -> Nullability b c -> Params c
param scalar dimensionality nullability = error "TODO"
