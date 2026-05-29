{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlCodecs.Columns where

import qualified Data.Attoparsec.Text as Atto
import PostgresqlCodecs.DecodingError
import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Scalar
import qualified PtrPeeker

-- * Ports

class (Applicative f) => ConsumesColumns f where
  consumeColumn ::
    Either DecodingError a ->
    PtrPeeker.Variable (Either DecodingError a) ->
    Atto.Parser a ->
    f a

-- * Abstraction

-- | Result set columns decoder.
data Columns a = Columns
  { -- | Expected number of columns in the result set.
    count :: Int32,
    -- | Wire-format decoder that supports both binary and text formats.
    decoder :: forall f. (ConsumesColumns f) => f a
  }

instance Functor Columns where
  fmap f Columns {count = cnt, decoder = dec} =
    Columns
      { count = cnt,
        decoder = fmap f dec
      }

instance Applicative Columns where
  pure x =
    Columns
      { count = 0,
        decoder = pure x
      }
  Columns {count = cf, decoder = df}
    <*> Columns {count = cx, decoder = dx} =
      Columns
        { count = cf + cx,
          decoder = df <*> dx
        }

column :: Scalar a -> Dimensionality a b -> Nullability b c -> Columns c
column scalar dimensionality nullability =
  let baseOid = fromMaybe 705 scalar.baseOid
      binPeek = dimensionality.binaryDecoder baseOid scalar.binaryDecoder
      txtParser = dimensionality.textualDecoder scalar.textualDecoder
      runOnNonNull = nullability.onNonNull
   in Columns
        { count = 1,
          decoder =
            consumeColumn
              nullability.onNull
              (fmap (fmap nullability.onNonNull) binPeek)
              (fmap runOnNonNull txtParser)
        }
