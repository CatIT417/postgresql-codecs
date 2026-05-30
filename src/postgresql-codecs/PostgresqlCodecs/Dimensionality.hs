module PostgresqlCodecs.Dimensionality where

import qualified Control.Monad.Trans.State as State
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import PostgresqlCodecs.Dimension
import qualified PostgresqlCodecs.Dimensionality.Writes as Writes
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Report (Report (..), toText)
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data EncodedArray = EncodedArray
  { encodedSizes :: [Int32],
    encodedWrite :: Write.Write
  }

data EncodedTextArray = EncodedTextArray
  { encodedTextSizes :: [Int32],
    encodedTextWrite :: TextBuilder.TextBuilder
  }

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

dimsMismatch :: Int32 -> Int32 -> Report
dimsMismatch expected actual =
  Report
    { message = "Dimension count mismatch",
      suggestion = "",
      details =
        [ ("details", "Expected " <> TextBuilder.toText (TextBuilder.decimal expected) <> " dimensions, but got " <> TextBuilder.toText (TextBuilder.decimal actual)),
          ("inputLocation", "[dimension-count]"),
          ("inputOffset", "0"),
          ("failingInput", TextBuilder.toText (TextBuilder.decimal actual)),
          ("expectedInputs", TextBuilder.toText (TextBuilder.decimal expected))
        ]
    }

baseOidMismatch :: Word32 -> Word32 -> Report
baseOidMismatch expected actual =
  Report
    { message = "Base OID mismatch",
      suggestion = "",
      details =
        [ ("details", "Expected " <> TextBuilder.toText (TextBuilder.decimal expected) <> ", but got " <> TextBuilder.toText (TextBuilder.decimal actual)),
          ("inputLocation", "[base-oid]"),
          ("inputOffset", "0"),
          ("failingInput", TextBuilder.toText (TextBuilder.decimal actual)),
          ("expectedInputs", TextBuilder.toText (TextBuilder.decimal expected))
        ]
    }

unexpectedNulls :: Report
unexpectedNulls =
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

unsupportedLowerBound :: Int32 -> Report
unsupportedLowerBound actual =
  Report
    { message = "Unsupported lower bound",
      suggestion = "",
      details =
        [ ("details", "Expected lower bound 1, but got " <> TextBuilder.toText (TextBuilder.decimal actual)),
          ("inputLocation", "[lower-bound]"),
          ("inputOffset", "0"),
          ("failingInput", TextBuilder.toText (TextBuilder.decimal actual)),
          ("expectedInputs", "1")
        ]
    }

rectangularArrayMismatch :: Report
rectangularArrayMismatch =
  Report
    { message = "Dimension size mismatch",
      suggestion = "",
      details =
        [ ("details", "All nested arrays must have the same dimensions"),
          ("inputLocation", "[dimension-sizes]"),
          ("inputOffset", "0"),
          ("failingInput", "")
        ]
    }

escapeArrayText :: Text.Text -> Text.Text
escapeArrayText =
  Text.concatMap \case
    '\\' -> "\\\\"
    '"' -> "\\\""
    c -> Text.singleton c

renderArrayText :: [TextBuilder.TextBuilder] -> TextBuilder.TextBuilder
renderArrayText elements =
  TextBuilder.text "{"
    <> mconcat (intersperse (TextBuilder.text ",") elements)
    <> TextBuilder.text "}"

dimsHeader :: Int32 -> Nullability a b -> Word32 -> ExceptT Report PtrPeeker.Variable [Int32]
dimsHeader expectedDimensionCount elementNullability expectedBaseOid = do
  (dimensionCount, hasNulls, baseOid) <- lift do
    PtrPeeker.fixed do
      dimensionCount <- PtrPeeker.beSignedInt4
      hasNulls <- do
        int <- PtrPeeker.beSignedInt4
        pure (int == 1)
      baseOid <- PtrPeeker.beUnsignedInt4
      pure (dimensionCount, hasNulls, baseOid)

  when (baseOid /= expectedBaseOid) do
    throwError (baseOidMismatch expectedBaseOid baseOid)

  when (dimensionCount /= expectedDimensionCount) do
    throwError (dimsMismatch expectedDimensionCount dimensionCount)

  when (hasNulls && not elementNullability.nullable) do
    throwError unexpectedNulls

  dimensions <- lift do
    PtrPeeker.fixed do
      replicateM (fromIntegral expectedDimensionCount) do
        size <- PtrPeeker.beSignedInt4
        lowerBound <- PtrPeeker.beSignedInt4
        pure (size, lowerBound)

  forM_ dimensions \(_, lowerBound) ->
    when (lowerBound /= 1) do
      throwError (unsupportedLowerBound lowerBound)

  pure (map fst dimensions)

encodeElement ::
  Nullability a b ->
  Word32 ->
  (a -> Either Report Write.Write) ->
  b ->
  Either Report Write.Write
encodeElement elementNullability _baseOid scalarEncoder =
  let Nullability {encoder = elementEncoder} = elementNullability
   in elementEncoder
        (Right (Write.bInt32 (-1)))
        ( \a -> do
            write <- scalarEncoder a
            Right (Write.bInt32 (fromIntegral (Write.writeSize write)) <> write)
        )

encode1 ::
  Nullability a b ->
  Dimension b c ->
  Word32 ->
  (a -> Either Report Write.Write) ->
  c ->
  Either Report EncodedArray
encode1 elementNullability (Dimension _ destruct1 count1) baseOid scalarEncoder vec = do
  body <-
    destruct1
      ( \acc element -> do
          acc' <- acc
          encoded <- encodeElement elementNullability baseOid scalarEncoder element
          pure (acc' <> encoded)
      )
      (Right mempty)
      vec
  pure
    EncodedArray
      { encodedSizes = [count1 vec],
        encodedWrite = body
      }

collectEncoded ::
  Int ->
  Int32 ->
  [EncodedArray] ->
  Either Report EncodedArray
collectEncoded dimensionCount outerCount encodedArrays =
  case encodedArrays of
    [] ->
      Right
        EncodedArray
          { encodedSizes = replicate dimensionCount 0,
            encodedWrite = mempty
          }
    EncodedArray {encodedSizes = sizes0, encodedWrite = write0} : rest ->
      let restSizes = map (\EncodedArray {encodedSizes = sizes} -> sizes) rest
          restWrites = map (\EncodedArray {encodedWrite = write} -> write) rest
       in if all (== sizes0) restSizes
            then
              Right
                EncodedArray
                  { encodedSizes = outerCount : sizes0,
                    encodedWrite = mconcat (write0 : restWrites)
                  }
            else Left rectangularArrayMismatch

collectTextEncoded ::
  Int ->
  Int32 ->
  [EncodedTextArray] ->
  Either Report EncodedTextArray
collectTextEncoded dimensionCount outerCount encodedArrays =
  case encodedArrays of
    [] ->
      Right
        EncodedTextArray
          { encodedTextSizes = replicate dimensionCount 0,
            encodedTextWrite = TextBuilder.text "{}"
          }
    EncodedTextArray {encodedTextSizes = sizes0, encodedTextWrite = write0} : rest ->
      let restSizes = map (\EncodedTextArray {encodedTextSizes = sizes} -> sizes) rest
          restWrites = map (\EncodedTextArray {encodedTextWrite = write} -> write) rest
       in if all (== sizes0) restSizes
            then
              Right
                EncodedTextArray
                  { encodedTextSizes = outerCount : sizes0,
                    encodedTextWrite = renderArrayText (write0 : restWrites)
                  }
            else Left rectangularArrayMismatch

encode2 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Word32 ->
  (a -> Either Report Write.Write) ->
  d ->
  Either Report EncodedArray
encode2 elementNullability dim1 (Dimension _ destruct2 count2) baseOid scalarEncoder vec = do
  rows <-
    destruct2
      ( \acc row -> do
          acc' <- acc
          encoded <- encode1 elementNullability dim1 baseOid scalarEncoder row
          pure (encoded : acc')
      )
      (Right [])
      vec
  collectEncoded 2 (count2 vec) (reverse rows)

encode3 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Word32 ->
  (a -> Either Report Write.Write) ->
  e ->
  Either Report EncodedArray
encode3 elementNullability dim1 dim2 (Dimension _ destruct3 count3) baseOid scalarEncoder vec = do
  rows <-
    destruct3
      ( \acc row -> do
          acc' <- acc
          encoded <- encode2 elementNullability dim1 dim2 baseOid scalarEncoder row
          pure (encoded : acc')
      )
      (Right [])
      vec
  collectEncoded 3 (count3 vec) (reverse rows)

encode4 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  Word32 ->
  (a -> Either Report Write.Write) ->
  f ->
  Either Report EncodedArray
encode4 elementNullability dim1 dim2 dim3 (Dimension _ destruct4 count4) baseOid scalarEncoder vec = do
  rows <-
    destruct4
      ( \acc row -> do
          acc' <- acc
          encoded <- encode3 elementNullability dim1 dim2 dim3 baseOid scalarEncoder row
          pure (encoded : acc')
      )
      (Right [])
      vec
  collectEncoded 4 (count4 vec) (reverse rows)

encode5 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  Dimension f g ->
  Word32 ->
  (a -> Either Report Write.Write) ->
  g ->
  Either Report EncodedArray
encode5 elementNullability dim1 dim2 dim3 dim4 (Dimension _ destruct5 count5) baseOid scalarEncoder vec = do
  rows <-
    destruct5
      ( \acc row -> do
          acc' <- acc
          encoded <- encode4 elementNullability dim1 dim2 dim3 dim4 baseOid scalarEncoder row
          pure (encoded : acc')
      )
      (Right [])
      vec
  collectEncoded 5 (count5 vec) (reverse rows)

encode6 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  Dimension f g ->
  Dimension g h ->
  Word32 ->
  (a -> Either Report Write.Write) ->
  h ->
  Either Report EncodedArray
encode6 elementNullability dim1 dim2 dim3 dim4 dim5 (Dimension _ destruct6 count6) baseOid scalarEncoder vec = do
  rows <-
    destruct6
      ( \acc row -> do
          acc' <- acc
          encoded <- encode5 elementNullability dim1 dim2 dim3 dim4 dim5 baseOid scalarEncoder row
          pure (encoded : acc')
      )
      (Right [])
      vec
  collectEncoded 6 (count6 vec) (reverse rows)

encodeTextElement ::
  Nullability a b ->
  (a -> Either Report TextBuilder.TextBuilder) ->
  b ->
  Either Report TextBuilder.TextBuilder
encodeTextElement elementNullability scalarEncoder =
  let Nullability {encoder = elementEncoder} = elementNullability
   in elementEncoder
        (Right (TextBuilder.text "NULL"))
        ( \a -> do
            text <- scalarEncoder a
            Right (TextBuilder.text "\"" <> TextBuilder.text (escapeArrayText (TextBuilder.toText text)) <> TextBuilder.text "\"")
        )

encodeText1 ::
  Nullability a b ->
  Dimension b c ->
  (a -> Either Report TextBuilder.TextBuilder) ->
  c ->
  Either Report EncodedTextArray
encodeText1 elementNullability (Dimension _ destruct1 count1) scalarEncoder vec = do
  elements <-
    destruct1
      ( \acc element -> do
          acc' <- acc
          encoded <- encodeTextElement elementNullability scalarEncoder element
          pure (encoded : acc')
      )
      (Right [])
      vec
  pure
    EncodedTextArray
      { encodedTextSizes = [count1 vec],
        encodedTextWrite = renderArrayText (reverse elements)
      }

encodeText2 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  (a -> Either Report TextBuilder.TextBuilder) ->
  d ->
  Either Report EncodedTextArray
encodeText2 elementNullability dim1 (Dimension _ destruct2 count2) scalarEncoder vec = do
  rows <-
    destruct2
      ( \acc row -> do
          acc' <- acc
          encoded <- encodeText1 elementNullability dim1 scalarEncoder row
          pure (encoded : acc')
      )
      (Right [])
      vec
  collectTextEncoded 2 (count2 vec) (reverse rows)

encodeText3 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  (a -> Either Report TextBuilder.TextBuilder) ->
  e ->
  Either Report EncodedTextArray
encodeText3 elementNullability dim1 dim2 (Dimension _ destruct3 count3) scalarEncoder vec = do
  rows <-
    destruct3
      ( \acc row -> do
          acc' <- acc
          encoded <- encodeText2 elementNullability dim1 dim2 scalarEncoder row
          pure (encoded : acc')
      )
      (Right [])
      vec
  collectTextEncoded 3 (count3 vec) (reverse rows)

encodeText4 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  (a -> Either Report TextBuilder.TextBuilder) ->
  f ->
  Either Report EncodedTextArray
encodeText4 elementNullability dim1 dim2 dim3 (Dimension _ destruct4 count4) scalarEncoder vec = do
  rows <-
    destruct4
      ( \acc row -> do
          acc' <- acc
          encoded <- encodeText3 elementNullability dim1 dim2 dim3 scalarEncoder row
          pure (encoded : acc')
      )
      (Right [])
      vec
  collectTextEncoded 4 (count4 vec) (reverse rows)

encodeText5 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  Dimension f g ->
  (a -> Either Report TextBuilder.TextBuilder) ->
  g ->
  Either Report EncodedTextArray
encodeText5 elementNullability dim1 dim2 dim3 dim4 (Dimension _ destruct5 count5) scalarEncoder vec = do
  rows <-
    destruct5
      ( \acc row -> do
          acc' <- acc
          encoded <- encodeText4 elementNullability dim1 dim2 dim3 dim4 scalarEncoder row
          pure (encoded : acc')
      )
      (Right [])
      vec
  collectTextEncoded 5 (count5 vec) (reverse rows)

encodeText6 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  Dimension f g ->
  Dimension g h ->
  (a -> Either Report TextBuilder.TextBuilder) ->
  h ->
  Either Report EncodedTextArray
encodeText6 elementNullability dim1 dim2 dim3 dim4 dim5 (Dimension _ destruct6 count6) scalarEncoder vec = do
  rows <-
    destruct6
      ( \acc row -> do
          acc' <- acc
          encoded <- encodeText5 elementNullability dim1 dim2 dim3 dim4 dim5 scalarEncoder row
          pure (encoded : acc')
      )
      (Right [])
      vec
  collectTextEncoded 6 (count6 vec) (reverse rows)

decodeElement ::
  Nullability a b ->
  PtrPeeker.Variable (Either Report a) ->
  ExceptT Report PtrPeeker.Variable b
decodeElement elementNullability scalarDecoder = ExceptT do
  contentSize <- PtrPeeker.fixed PtrPeeker.beSignedInt4
  case contentSize of
    -1 -> pure elementNullability.onNull
    _ -> fmap (fmap elementNullability.onNonNull) scalarDecoder

decode1 ::
  Nullability a b ->
  Dimension b c ->
  PtrPeeker.Variable (Either Report a) ->
  Int32 ->
  ExceptT Report PtrPeeker.Variable c
decode1 elementNullability (Dimension construct1 _ _) scalarDecoder size1 =
  construct1 (fromIntegral size1) (decodeElement elementNullability scalarDecoder)

decode2 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  PtrPeeker.Variable (Either Report a) ->
  Int32 ->
  Int32 ->
  ExceptT Report PtrPeeker.Variable d
decode2 elementNullability dim1 (Dimension construct2 _ _) scalarDecoder size1 size2 =
  construct2 (fromIntegral size1) (decode1 elementNullability dim1 scalarDecoder size2)

decode3 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  PtrPeeker.Variable (Either Report a) ->
  Int32 ->
  Int32 ->
  Int32 ->
  ExceptT Report PtrPeeker.Variable e
decode3 elementNullability dim1 dim2 (Dimension construct3 _ _) scalarDecoder size1 size2 size3 =
  construct3 (fromIntegral size1) (decode2 elementNullability dim1 dim2 scalarDecoder size2 size3)

decode4 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  PtrPeeker.Variable (Either Report a) ->
  Int32 ->
  Int32 ->
  Int32 ->
  Int32 ->
  ExceptT Report PtrPeeker.Variable f
decode4 elementNullability dim1 dim2 dim3 (Dimension construct4 _ _) scalarDecoder size1 size2 size3 size4 =
  construct4 (fromIntegral size1) (decode3 elementNullability dim1 dim2 dim3 scalarDecoder size2 size3 size4)

decode5 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  Dimension f g ->
  PtrPeeker.Variable (Either Report a) ->
  Int32 ->
  Int32 ->
  Int32 ->
  Int32 ->
  Int32 ->
  ExceptT Report PtrPeeker.Variable g
decode5 elementNullability dim1 dim2 dim3 dim4 (Dimension construct5 _ _) scalarDecoder size1 size2 size3 size4 size5 =
  construct5 (fromIntegral size1) (decode4 elementNullability dim1 dim2 dim3 dim4 scalarDecoder size2 size3 size4 size5)

decode6 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  Dimension f g ->
  Dimension g h ->
  PtrPeeker.Variable (Either Report a) ->
  Int32 ->
  Int32 ->
  Int32 ->
  Int32 ->
  Int32 ->
  Int32 ->
  ExceptT Report PtrPeeker.Variable h
decode6 elementNullability dim1 dim2 dim3 dim4 dim5 (Dimension construct6 _ _) scalarDecoder size1 size2 size3 size4 size5 size6 =
  construct6 (fromIntegral size1) (decode5 elementNullability dim1 dim2 dim3 dim4 dim5 scalarDecoder size2 size3 size4 size5 size6)

buildFromList ::
  Dimension a b ->
  [a] ->
  Atto.Parser b
buildFromList Dimension {construct} values =
  State.evalStateT (construct (length values) nextValue) values
  where
    nextValue =
      State.state \case
        [] -> error "impossible: exhausted array values"
        x : xs -> (x, xs)

parseQuotedArrayText :: Atto.Parser Text.Text
parseQuotedArrayText = do
  _ <- Atto.char '"'
  pieces <- Atto.many' (escapedPiece <|> plainPiece)
  _ <- Atto.char '"'
  pure (Text.concat pieces)
  where
    escapedPiece = do
      _ <- Atto.char '\\'
      c <- Atto.anyChar
      pure (Text.singleton c)
    plainPiece = Atto.takeWhile1 (\c -> c /= '"' && c /= '\\')

parseArrayToken :: Atto.Parser (Maybe Text.Text)
parseArrayToken =
  Atto.choice
    [ Just <$> parseQuotedArrayText,
      do
        token <- Atto.takeWhile1 (\c -> c /= ',' && c /= '}')
        pure (if token == "NULL" then Nothing else Just token)
    ]

parseTextElement ::
  Nullability a b ->
  Atto.Parser a ->
  Atto.Parser b
parseTextElement elementNullability scalarDecoder = do
  token <- parseArrayToken
  case token of
    Nothing ->
      either (fail . Text.unpack . toText) pure elementNullability.onNull
    Just text ->
      case Atto.parseOnly (scalarDecoder <* Atto.endOfInput) text of
        Left err -> fail err
        Right value -> pure (elementNullability.onNonNull value)

parseTextArray1 ::
  Nullability a b ->
  Dimension b c ->
  Atto.Parser a ->
  Atto.Parser c
parseTextArray1 elementNullability dim1 scalarDecoder = do
  values <- Atto.char '{' *> Atto.sepBy (parseTextElement elementNullability scalarDecoder) (Atto.char ',') <* Atto.char '}'
  buildFromList dim1 values

parseTextArray2 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Atto.Parser a ->
  Atto.Parser d
parseTextArray2 elementNullability dim1 dim2 scalarDecoder = do
  values <- Atto.char '{' *> Atto.sepBy (parseTextArray1 elementNullability dim1 scalarDecoder) (Atto.char ',') <* Atto.char '}'
  buildFromList dim2 values

parseTextArray3 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Atto.Parser a ->
  Atto.Parser e
parseTextArray3 elementNullability dim1 dim2 dim3 scalarDecoder = do
  values <- Atto.char '{' *> Atto.sepBy (parseTextArray2 elementNullability dim1 dim2 scalarDecoder) (Atto.char ',') <* Atto.char '}'
  buildFromList dim3 values

parseTextArray4 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  Atto.Parser a ->
  Atto.Parser f
parseTextArray4 elementNullability dim1 dim2 dim3 dim4 scalarDecoder = do
  values <- Atto.char '{' *> Atto.sepBy (parseTextArray3 elementNullability dim1 dim2 dim3 scalarDecoder) (Atto.char ',') <* Atto.char '}'
  buildFromList dim4 values

parseTextArray5 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  Dimension f g ->
  Atto.Parser a ->
  Atto.Parser g
parseTextArray5 elementNullability dim1 dim2 dim3 dim4 dim5 scalarDecoder = do
  values <- Atto.char '{' *> Atto.sepBy (parseTextArray4 elementNullability dim1 dim2 dim3 dim4 scalarDecoder) (Atto.char ',') <* Atto.char '}'
  buildFromList dim5 values

parseTextArray6 ::
  Nullability a b ->
  Dimension b c ->
  Dimension c d ->
  Dimension d e ->
  Dimension e f ->
  Dimension f g ->
  Dimension g h ->
  Atto.Parser a ->
  Atto.Parser h
parseTextArray6 elementNullability dim1 dim2 dim3 dim4 dim5 dim6 scalarDecoder = do
  values <- Atto.char '{' *> Atto.sepBy (parseTextArray5 elementNullability dim1 dim2 dim3 dim4 dim5 scalarDecoder) (Atto.char ',') <* Atto.char '}'
  buildFromList dim6 values

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
  Dimensionality
    { dims = 1,
      binaryEncoder = \baseOid scalarEncoder vec ->
        let body = encode1 elementNullability (Dimension construct1 destruct1 count1) baseOid scalarEncoder vec
         in fmap (\EncodedArray {..} -> Writes.arrayHeader 1 elementNullability.nullable baseOid encodedSizes <> encodedWrite) body,
      binaryDecoder = \expectedBaseOid scalarDecoder -> runExceptT do
        sizes <- dimsHeader 1 elementNullability expectedBaseOid
        case sizes of
          [size1] -> decode1 elementNullability (Dimension construct1 destruct1 count1) scalarDecoder size1
          _ -> throwError (dimsMismatch 1 (fromIntegral (length sizes))),
      textualEncoder = \scalarEncoder vec ->
        fmap (\EncodedTextArray {..} -> encodedTextWrite) (encodeText1 elementNullability (Dimension construct1 destruct1 count1) scalarEncoder vec),
      textualDecoder = \scalarDecoder ->
        parseTextArray1 elementNullability (Dimension construct1 destruct1 count1) scalarDecoder
    }

d2 :: Nullability a b -> Dimension b c -> Dimension c d -> Dimensionality a d
d2 elementNullability dim1 dim2 =
  Dimensionality
    { dims = 2,
      binaryEncoder = \baseOid scalarEncoder vec ->
        let body = encode2 elementNullability dim1 dim2 baseOid scalarEncoder vec
         in fmap (\EncodedArray {..} -> Writes.arrayHeader 2 elementNullability.nullable baseOid encodedSizes <> encodedWrite) body,
      binaryDecoder = \expectedBaseOid scalarDecoder -> runExceptT do
        sizes <- dimsHeader 2 elementNullability expectedBaseOid
        case sizes of
          [size1, size2] -> decode2 elementNullability dim1 dim2 scalarDecoder size1 size2
          _ -> throwError (dimsMismatch 2 (fromIntegral (length sizes))),
      textualEncoder = \scalarEncoder vec ->
        fmap (\EncodedTextArray {..} -> encodedTextWrite) (encodeText2 elementNullability dim1 dim2 scalarEncoder vec),
      textualDecoder = \scalarDecoder ->
        parseTextArray2 elementNullability dim1 dim2 scalarDecoder
    }

d3 :: Nullability a b -> Dimension b c -> Dimension c d -> Dimension d e -> Dimensionality a e
d3 elementNullability dim1 dim2 dim3 =
  Dimensionality
    { dims = 3,
      binaryEncoder = \baseOid scalarEncoder vec ->
        let body = encode3 elementNullability dim1 dim2 dim3 baseOid scalarEncoder vec
         in fmap (\EncodedArray {..} -> Writes.arrayHeader 3 elementNullability.nullable baseOid encodedSizes <> encodedWrite) body,
      binaryDecoder = \expectedBaseOid scalarDecoder -> runExceptT do
        sizes <- dimsHeader 3 elementNullability expectedBaseOid
        case sizes of
          [size1, size2, size3] -> decode3 elementNullability dim1 dim2 dim3 scalarDecoder size1 size2 size3
          _ -> throwError (dimsMismatch 3 (fromIntegral (length sizes))),
      textualEncoder = \scalarEncoder vec ->
        fmap (\EncodedTextArray {..} -> encodedTextWrite) (encodeText3 elementNullability dim1 dim2 dim3 scalarEncoder vec),
      textualDecoder = \scalarDecoder ->
        parseTextArray3 elementNullability dim1 dim2 dim3 scalarDecoder
    }

d4 :: Nullability a b -> Dimension b c -> Dimension c d -> Dimension d e -> Dimension e f -> Dimensionality a f
d4 elementNullability dim1 dim2 dim3 dim4 =
  Dimensionality
    { dims = 4,
      binaryEncoder = \baseOid scalarEncoder vec ->
        let body = encode4 elementNullability dim1 dim2 dim3 dim4 baseOid scalarEncoder vec
         in fmap (\EncodedArray {..} -> Writes.arrayHeader 4 elementNullability.nullable baseOid encodedSizes <> encodedWrite) body,
      binaryDecoder = \expectedBaseOid scalarDecoder -> runExceptT do
        sizes <- dimsHeader 4 elementNullability expectedBaseOid
        case sizes of
          [size1, size2, size3, size4] -> decode4 elementNullability dim1 dim2 dim3 dim4 scalarDecoder size1 size2 size3 size4
          _ -> throwError (dimsMismatch 4 (fromIntegral (length sizes))),
      textualEncoder = \scalarEncoder vec ->
        fmap (\EncodedTextArray {..} -> encodedTextWrite) (encodeText4 elementNullability dim1 dim2 dim3 dim4 scalarEncoder vec),
      textualDecoder = \scalarDecoder ->
        parseTextArray4 elementNullability dim1 dim2 dim3 dim4 scalarDecoder
    }

d5 :: Nullability a b -> Dimension b c -> Dimension c d -> Dimension d e -> Dimension e f -> Dimension f g -> Dimensionality a g
d5 elementNullability dim1 dim2 dim3 dim4 dim5 =
  Dimensionality
    { dims = 5,
      binaryEncoder = \baseOid scalarEncoder vec ->
        let body = encode5 elementNullability dim1 dim2 dim3 dim4 dim5 baseOid scalarEncoder vec
         in fmap (\EncodedArray {..} -> Writes.arrayHeader 5 elementNullability.nullable baseOid encodedSizes <> encodedWrite) body,
      binaryDecoder = \expectedBaseOid scalarDecoder -> runExceptT do
        sizes <- dimsHeader 5 elementNullability expectedBaseOid
        case sizes of
          [size1, size2, size3, size4, size5] -> decode5 elementNullability dim1 dim2 dim3 dim4 dim5 scalarDecoder size1 size2 size3 size4 size5
          _ -> throwError (dimsMismatch 5 (fromIntegral (length sizes))),
      textualEncoder = \scalarEncoder vec ->
        fmap (\EncodedTextArray {..} -> encodedTextWrite) (encodeText5 elementNullability dim1 dim2 dim3 dim4 dim5 scalarEncoder vec),
      textualDecoder = \scalarDecoder ->
        parseTextArray5 elementNullability dim1 dim2 dim3 dim4 dim5 scalarDecoder
    }

d6 :: Nullability a b -> Dimension b c -> Dimension c d -> Dimension d e -> Dimension e f -> Dimension f g -> Dimension g h -> Dimensionality a h
d6 elementNullability dim1 dim2 dim3 dim4 dim5 dim6 =
  Dimensionality
    { dims = 6,
      binaryEncoder = \baseOid scalarEncoder vec ->
        let body = encode6 elementNullability dim1 dim2 dim3 dim4 dim5 dim6 baseOid scalarEncoder vec
         in fmap (\EncodedArray {..} -> Writes.arrayHeader 6 elementNullability.nullable baseOid encodedSizes <> encodedWrite) body,
      binaryDecoder = \expectedBaseOid scalarDecoder -> runExceptT do
        sizes <- dimsHeader 6 elementNullability expectedBaseOid
        case sizes of
          [size1, size2, size3, size4, size5, size6] -> decode6 elementNullability dim1 dim2 dim3 dim4 dim5 dim6 scalarDecoder size1 size2 size3 size4 size5 size6
          _ -> throwError (dimsMismatch 6 (fromIntegral (length sizes))),
      textualEncoder = \scalarEncoder vec ->
        fmap (\EncodedTextArray {..} -> encodedTextWrite) (encodeText6 elementNullability dim1 dim2 dim3 dim4 dim5 dim6 scalarEncoder vec),
      textualDecoder = \scalarDecoder ->
        parseTextArray6 elementNullability dim1 dim2 dim3 dim4 dim5 dim6 scalarDecoder
    }
