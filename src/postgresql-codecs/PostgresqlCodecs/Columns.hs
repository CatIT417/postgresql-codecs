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

class (Applicative f) => BinaryDecoder f where
  consumeColumnInBinary ::
    Either DecodingError a ->
    PtrPeeker.Variable (Either DecodingError a) ->
    f a

class (Applicative f) => TextDecoder f where
  consumeColumnInText ::
    Either DecodingError a ->
    (Text -> Either DecodingError a) ->
    f a

-- * Abstraction

-- | Result set columns decoder.
data Columns a = Columns
  { -- | Expected number of columns in the result set.
    count :: Int32,
    -- | Wire-format binary decoder.
    binaryDecoder :: forall f. (BinaryDecoder f) => f a,
    -- | Wire-format text decoder.
    textDecoder :: forall f. (TextDecoder f) => f a
  }

instance Functor Columns

instance Applicative Columns

column :: Scalar a -> Dimensionality a b -> Nullability b c -> Columns c
column scalar dimensionality nullability =
  let baseOid = fromMaybe 705 scalar.baseOid
      binPeek = dimensionality.binaryDecoder baseOid scalar.binaryDecoder
      txtParser = dimensionality.textualDecoder scalar.textualDecoder
   in Columns
        { count = 1,
          binaryDecoder =
            consumeColumnInBinary
              nullability.onNull
              (fmap (>>= nullability.onNonNull) binPeek),
          textDecoder =
            consumeColumnInText nullability.onNull \text ->
              case Atto.parseOnly txtParser text of
                Left err ->
                  Left
                    DecodingError
                      { message = fromString err,
                        details = "",
                        row = Nothing,
                        column = Nothing,
                        inputLocation = ["surface-text"],
                        inputOffset = 0,
                        failingInput = text,
                        expectedInputs = Nothing
                      }
                Right val -> nullability.onNonNull val
        }
