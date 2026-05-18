{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Columns where

import qualified Data.Attoparsec.Text as Atto
import PostgresqlCodecs.DecodingError
import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Scalar
import qualified PostgresqlTypes.Algebra
import qualified PtrPeeker

-- * Ports

-- | A driver-specific execution context.
class (Applicative f) => BinaryDecoder f where
  -- | Read the next column value.
  decodeColumnInBinary :: PtrPeeker.Variable (Either DecodingError a) -> f a

-- | A driver-specific execution context.
class (Applicative f) => TextDecoder f where
  -- | Read the next column value.
  decodeColumnInText :: Atto.Parser a -> f a

-- * Abstraction

-- | Result set columns decoder.
data Columns a = Columns
  { -- | Expected number of columns in the result set. This is used to verify that the number of columns in the result set matches the expected number prior to attempting to decode the columns.
    count :: Int32,
    binaryDecoder :: forall f. (BinaryDecoder f) => f a,
    textDecoder :: forall f. (TextDecoder f) => f a
  }

instance Functor Columns

instance Applicative Columns

column :: Scalar a -> Dimensionality a b -> Nullability b c -> Columns c
column scalar dimensionality nullability =
  let baseOid = fromMaybe 705 scalar.baseOid
   in Columns
        { count = 1,
          binaryDecoder =
            decodeColumnInBinary
              ( nullability.binaryDecoderWrapper
                  ( dimensionality.binaryDecoder
                      baseOid
                      scalar.binaryDecoder
                  )
              ),
          textDecoder =
            decodeColumnInText do
              fail "Text column decoding is not implemented for this scalar codec"
        }
