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
import qualified TextBuilder

-- * Ports

-- | A driver-specific execution context for wire-format decoding.
class (Applicative f) => BinaryDecoder f where
  -- | Read the next column value from the wire format.
  decodeColumnInBinary :: PtrPeeker.Variable (Either DecodingError a) -> f a

-- | A driver-specific execution context for wire-format decoding.
class (Applicative f) => TextDecoder f where
  -- | Read the next column value from the wire format.
  decodeColumnInText :: Atto.Parser a -> f a

-- * Abstraction

-- | Result set columns decoder.
data Columns a = Columns
  { -- | Expected number of columns in the result set.
    count :: Int32,
    -- | Wire-format binary decoder.
    binaryDecoder :: forall f. (BinaryDecoder f) => f a,
    -- | Wire-format text decoder.
    textDecoder :: forall f. (TextDecoder f) => f a,
    -- | Surface binary decoder for driver APIs that provide @Maybe ByteString@ (e.g., libpq).
    binarySurfaceDecoder :: Maybe ByteString -> Either DecodingError a,
    -- | Surface text decoder for driver APIs that provide @Maybe Text@ (e.g., libpq).
    textSurfaceDecoder :: Maybe Text -> Either DecodingError a
  }

instance Functor Columns

instance Applicative Columns

column :: Scalar a -> Dimensionality a b -> Nullability b c -> Columns c
column scalar dimensionality nullability =
  let baseOid = fromMaybe 705 scalar.baseOid
      binPeek = dimensionality.binaryDecoder baseOid scalar.binaryDecoder
      txtParser = dimensionality.textualDecoder scalar.textualDecoder
      runBin bs =
        case PtrPeeker.runVariableOnByteString binPeek bs of
          Left n ->
            Left
              DecodingError
                { message = "Insufficient bytes",
                  details = "Need " <> TextBuilder.toText (TextBuilder.decimal n) <> " more bytes",
                  row = Nothing,
                  column = Nothing,
                  inputLocation = ["surface-binary"],
                  inputOffset = 0,
                  failingInput = "",
                  expectedInputs = Nothing
                }
          Right (Left err) -> Left err
          Right (Right val) -> Right val
      runTxt text =
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
          Right val -> Right val
   in Columns
        { count = 1,
          binaryDecoder =
            decodeColumnInBinary
              (nullability.binaryDecoderWrapper binPeek),
          textDecoder =
            decodeColumnInText
              (nullability.textualDecoderWrapper txtParser),
          binarySurfaceDecoder = nullability.binarySurfaceDecoder runBin,
          textSurfaceDecoder = nullability.textualSurfaceDecoder runTxt
        }
