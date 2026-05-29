module PostgresqlCodecs.Dimensionality where

import qualified Data.Attoparsec.Text as Atto
import PostgresqlCodecs.Dimension
import qualified PostgresqlCodecs.Dimensionality.Writes as Writes
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Report (Report (..))
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data Dimensionality scalar vec = Dimensionality
  { dims :: Int,
    binaryEncoder ::
      Word32 ->
      (scalar -> Either Report Write.Write) ->
      (vec -> Either Report Write.Write),
    binaryDecoder ::
      Word32 ->
      PtrPeeker.Variable (Either Report scalar) ->
      PtrPeeker.Variable (Either Report vec),
    textualEncoder ::
      (scalar -> Either Report TextBuilder.TextBuilder) ->
      (vec -> Either Report TextBuilder.TextBuilder),
    textualDecoder ::
      Atto.Parser scalar ->
      Atto.Parser vec
  }

d0 :: Dimensionality a a
d0 =
  Dimensionality
    { dims = 0,
      binaryEncoder = const id,
      binaryDecoder = const id,
      textualEncoder = id,
      textualDecoder = id
    }

d1 :: Nullability a b -> Dimension b c -> Dimensionality a c
d1 elementNullability (Dimension construct1 destruct1 count1) =
  let Nullability {encoder = elementEncoder} = elementNullability
   in Dimensionality
        { dims = 1,
          binaryEncoder =
            \baseOid scalarEncoder vec ->
              let head =
                    Writes.arrayHeader 1 elementNullability.nullable baseOid [count1 vec]
                  elementEnc =
                    \element ->
                      elementEncoder
                        (Right (Write.bInt32 (-1)))
                        ( \a -> do
                            write <- scalarEncoder a
                            Right (Write.bInt32 (fromIntegral (Write.writeSize write)) <> write)
                        )
                        element
                  body =
                    destruct1
                      ( \accumulator element -> do
                          acc <- accumulator
                          encoded <- elementEnc element
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
                  Report
                    { message = "Base OID mismatch",
                      suggestion = "",
                      details =
                        [ ("details", "Expected " <> TextBuilder.toText (TextBuilder.decimal expectedBaseOid) <> ", but got " <> TextBuilder.toText (TextBuilder.decimal baseOid)),
                          ("inputLocation", "[base-oid]"),
                          ("inputOffset", "0"),
                          ("failingInput", TextBuilder.toText (TextBuilder.decimal baseOid)),
                          ("expectedInputs", TextBuilder.toText (TextBuilder.decimal expectedBaseOid))
                        ]
                    }

              -- Interrupt early if the amount of dimensions is wrong.
              when (dimensionCount /= 1) do
                throwError
                  Report
                    { message = "Dimension count mismatch",
                      suggestion = "",
                      details =
                        [ ("details", "Expected 1 dimension, but got " <> TextBuilder.toText (TextBuilder.decimal dimensionCount)),
                          ("inputLocation", "[dimension-count]"),
                          ("inputOffset", "0"),
                          ("failingInput", TextBuilder.toText (TextBuilder.decimal dimensionCount)),
                          ("expectedInputs", "1")
                        ]
                    }

              -- Interrupt early if the array contains nulls and they are not expected.
              when (hasNulls && not elementNullability.nullable) do
                throwError
                  Report
                    { message = "Unexpected nulls in array",
                      suggestion = "",
                      details =
                        [ ("details", "Array contains nulls, but element type is non-nullable"),
                          ("inputLocation", "[contains-nulls]"),
                          ("inputOffset", "0"),
                          ("failingInput", "")
                        ]
                    }

              (size1, lowerBound1) <- lift do
                PtrPeeker.fixed do
                  liftA2 (,) PtrPeeker.beSignedInt4 PtrPeeker.beSignedInt4

              -- Interrupt early if the lower-bound value is not supported.
              when (lowerBound1 /= 1) do
                throwError
                  Report
                    { message = "Unsupported lower bound",
                      suggestion = "",
                      details =
                        [ ("details", "Expected lower bound 1, but got " <> TextBuilder.toText (TextBuilder.decimal lowerBound1)),
                          ("inputLocation", "[lower-bound]"),
                          ("inputOffset", "0"),
                          ("failingInput", TextBuilder.toText (TextBuilder.decimal lowerBound1)),
                          ("expectedInputs", "1")
                        ]
                    }

              construct1 (fromIntegral size1) do
                ExceptT do
                  contentSize <- PtrPeeker.fixed PtrPeeker.beSignedInt4
                  case contentSize of
                    -1 -> pure elementNullability.onNull
                    _ ->
                      fmap (fmap elementNullability.onNonNull) scalarDecoder,
          textualEncoder = \_scalarEncoder _vec ->
            error "TODO",
          textualDecoder = \_scalarDecoder ->
            error "TODO"
        }

d2 :: Nullability a b -> Dimension b c -> Dimension c d -> Dimensionality a d
d2 = error "TODO"
