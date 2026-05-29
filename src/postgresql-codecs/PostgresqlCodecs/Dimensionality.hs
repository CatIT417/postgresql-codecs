module PostgresqlCodecs.Dimensionality where

import qualified Data.Attoparsec.Text as Atto
import PostgresqlCodecs.DecodingError
import PostgresqlCodecs.Dimension
import qualified PostgresqlCodecs.Dimensionality.Writes as Writes
import PostgresqlCodecs.EncodingError
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data Dimensionality scalar vec = Dimensionality
  { binaryEncoder ::
      Word32 ->
      (scalar -> Either EncodingError Write.Write) ->
      (vec -> Either EncodingError Write.Write),
    binaryDecoder ::
      Word32 ->
      PtrPeeker.Variable (Either DecodingError scalar) ->
      PtrPeeker.Variable (Either DecodingError vec),
    textualEncoder ::
      (scalar -> Either EncodingError TextBuilder.TextBuilder) ->
      (vec -> Either EncodingError TextBuilder.TextBuilder),
    textualDecoder ::
      Atto.Parser scalar ->
      Atto.Parser vec
  }

d0 :: Dimensionality a a
d0 =
  Dimensionality
    { binaryEncoder = const id,
      binaryDecoder = const id,
      textualEncoder = id,
      textualDecoder = id
    }

d1 :: Nullability a b -> Dimension b c -> Dimensionality a c
d1 elementNullability (Dimension construct1 destruct1 count1) =
  Dimensionality
    { binaryEncoder =
        \baseOid scalarEncoder vec ->
          let head =
                Writes.arrayHeader 1 elementNullability.nullable baseOid [count1 vec]
              elementEncoder =
                elementNullability.binaryEncoder scalarEncoder
              body =
                destruct1
                  ( \accumulator element -> do
                      acc <- accumulator
                      mEncoded <- elementEncoder element
                      let encoded = case mEncoded of
                            Nothing -> Write.bInt32 (-1)
                            Just write ->
                              Write.bInt32 (fromIntegral (Write.writeSize write)) <> write
                      Right (acc <> encoded)
                  )
                  (Right mempty)
                  vec
           in fmap (head <>) body,
      binaryDecoder =
        \expectedBaseOid scalarDecoder -> runExceptT do
          (dimensionCount, hasNulls, baseOid) <- lift do
            PtrPeeker.fixed do
              dimensionCount <- PtrPeeker.beSignedInt4
              hasNulls <- do
                int <- PtrPeeker.beSignedInt4
                pure (int == 1)
              baseOid <- PtrPeeker.beUnsignedInt4
              pure (dimensionCount, hasNulls, baseOid)

          -- Interrupt early if baseOid does not match the expected one.
          when (baseOid /= expectedBaseOid) do
            throwError
              DecodingError
                { message = "Base OID mismatch",
                  details = "Expected " <> TextBuilder.toText (TextBuilder.decimal expectedBaseOid) <> ", but got " <> TextBuilder.toText (TextBuilder.decimal baseOid),
                  row = Nothing,
                  column = Nothing,
                  inputLocation = ["base-oid"],
                  inputOffset = 0,
                  failingInput = TextBuilder.toText (TextBuilder.decimal baseOid),
                  expectedInputs = Just [TextBuilder.toText (TextBuilder.decimal expectedBaseOid)]
                }

          -- Interrupt early if the amount of dimensions is wrong.
          when (dimensionCount /= 1) do
            throwError
              DecodingError
                { message = "Dimension count mismatch",
                  details = "Expected 1 dimension, but got " <> TextBuilder.toText (TextBuilder.decimal dimensionCount),
                  row = Nothing,
                  column = Nothing,
                  inputLocation = ["dimension-count"],
                  inputOffset = 0,
                  failingInput = TextBuilder.toText (TextBuilder.decimal dimensionCount),
                  expectedInputs = Just ["1"]
                }

          -- Interrupt early if the array contains nulls and they are not expected.
          when (hasNulls && not elementNullability.nullable) do
            throwError
              DecodingError
                { message = "Unexpected nulls in array",
                  details = "Array contains nulls, but element type is non-nullable",
                  row = Nothing,
                  column = Nothing,
                  inputLocation = ["contains-nulls"],
                  inputOffset = 0,
                  failingInput = "",
                  expectedInputs = Nothing
                }

          (size1, lowerBound1) <- lift do
            PtrPeeker.fixed do
              liftA2 (,) PtrPeeker.beSignedInt4 PtrPeeker.beSignedInt4

          -- Interrupt early if the lower-bound value is not supported.
          when (lowerBound1 /= 1) do
            throwError
              DecodingError
                { message = "Unsupported lower bound",
                  details = "Expected lower bound 1, but got " <> TextBuilder.toText (TextBuilder.decimal lowerBound1),
                  row = Nothing,
                  column = Nothing,
                  inputLocation = ["lower-bound"],
                  inputOffset = 0,
                  failingInput = TextBuilder.toText (TextBuilder.decimal lowerBound1),
                  expectedInputs = Just ["1"]
                }

          construct1 (fromIntegral size1) do
            ExceptT do
              contentSize <- PtrPeeker.fixed PtrPeeker.beSignedInt4
              case contentSize of
                -1 -> pure elementNullability.onNull
                _ ->
                  fmap (>>= elementNullability.onNonNull) scalarDecoder,
      textualEncoder = \_scalarEncoder _vec ->
        error "TODO",
      textualDecoder = \_scalarDecoder ->
        error "TODO"
    }

d2 :: Nullability a b -> Dimension b c -> Dimension c d -> Dimensionality a d
d2 = error "TODO"
