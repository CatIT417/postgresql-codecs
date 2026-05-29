module PostgresqlCodecs.Fields where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Report (Report (..))
import PostgresqlCodecs.Scalar
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data Fields a b = Fields
  { count :: Int32,
    binaryEncoder :: a -> Either Report Write.Write,
    binaryDecoder :: PtrPeeker.Variable (Either Report b),
    -- | Represent in Postgres textual format.
    textualEncoder :: a -> Either Report [TextBuilder.TextBuilder],
    textualDecoder :: Atto.Parser b
  }

instance Functor (Fields a) where
  fmap f Fields {..} =
    Fields
      { count,
        binaryEncoder,
        binaryDecoder = fmap (fmap f) binaryDecoder,
        textualEncoder,
        textualDecoder = fmap f textualDecoder
      }

instance Applicative (Fields a) where
  pure x =
    Fields
      { count = 0,
        binaryEncoder = const (Right mempty),
        binaryDecoder = pure (Right x),
        textualEncoder = const (Right []),
        textualDecoder = pure x
      }
  (<*>) left right =
    let textualDecoder =
          if left.count == 0 || right.count == 0
            then left.textualDecoder <*> right.textualDecoder
            else left.textualDecoder <*> (Atto.char ',' *> right.textualDecoder)
     in Fields
          { count = left.count + right.count,
            binaryEncoder = \value -> liftA2 (<>) (left.binaryEncoder value) (right.binaryEncoder value),
            binaryDecoder = liftA2 (<*>) left.binaryDecoder right.binaryDecoder,
            textualEncoder = \value -> do
              leftTexts <- left.textualEncoder value
              rightTexts <- right.textualEncoder value
              Right (leftTexts <> rightTexts),
            textualDecoder
          }

instance Profunctor Fields where
  dimap f g Fields {..} =
    Fields
      { count,
        binaryEncoder = \value -> binaryEncoder (f value),
        binaryDecoder = fmap (fmap g) binaryDecoder,
        textualEncoder = \value -> textualEncoder (f value),
        textualDecoder = fmap g textualDecoder
      }

field :: Scalar a -> Dimensionality a b -> Nullability b c -> Fields c c
field scalar dimensionality nullability =
  let baseOid = fromMaybe 705 scalar.baseOid
      binEnc = dimensionality.binaryEncoder baseOid scalar.binaryEncoder
      txtEnc = dimensionality.textualEncoder scalar.textualEncoder
      Nullability {encoder = enc} = nullability
   in Fields
        { count = 1,
          binaryEncoder = \value ->
            let oid = Write.bInt32 (fromIntegral baseOid)
             in enc
                  (Right (oid <> Write.bInt32 (-1)))
                  ( \a -> do
                      write <- binEnc a
                      let size = Write.bInt32 (fromIntegral (Write.writeSize write))
                      Right (oid <> size <> write)
                  )
                  value,
          binaryDecoder = do
            _fieldOid <- PtrPeeker.fixed PtrPeeker.beSignedInt4
            contentSize <- PtrPeeker.fixed PtrPeeker.beSignedInt4
            case contentSize of
              -1 -> pure nullability.onNull
              _ ->
                fmap
                  (fmap nullability.onNonNull)
                  (dimensionality.binaryDecoder baseOid scalar.binaryDecoder),
          textualEncoder = \value ->
            enc
              (Right [mempty])
              (\a -> do text <- txtEnc a; Right [text])
              value,
          textualDecoder = do
            rawField <- compositeFieldRawText
            case rawField of
              Nothing ->
                case nullability.onNull of
                  Left report -> fail (Text.unpack report.message)
                  Right value -> pure value
              Just fieldText -> do
                normalizedFieldText <-
                  case decodeQuotedCompositeFieldText fieldText of
                    Left message -> fail message
                    Right text -> pure text
                case Atto.parseOnly (dimensionality.textualDecoder scalar.textualDecoder <* Atto.endOfInput) normalizedFieldText of
                  Left message -> fail message
                  Right value -> pure (nullability.onNonNull value)
        }

composite :: Text -> Text -> Fields a a -> Scalar a
composite schemaName typeName fields =
  Scalar
    { schemaName,
      typeName,
      baseOid = Nothing,
      arrayOid = Nothing,
      supportsBinary = True,
      binaryEncoder = \value -> do
        fieldWrites <- fields.binaryEncoder value
        Right (Write.bInt32 fields.count <> fieldWrites),
      binaryDecoder =
        runExceptT do
          _ <-
            ExceptT do
              PtrPeeker.fixed do
                PtrPeeker.beSignedInt4
                  <&> \count ->
                    if count == fields.count
                      then Right ()
                      else
                        Left
                          Report
                            { message = "Field count mismatch",
                              suggestion = "",
                              details =
                                [ ("details", "Expected " <> TextBuilder.toText (TextBuilder.decimal fields.count) <> " fields, but got " <> TextBuilder.toText (TextBuilder.decimal count)),
                                  ("inputLocation", "[field-count]"),
                                  ("inputOffset", "0"),
                                  ("failingInput", TextBuilder.toText (TextBuilder.decimal count)),
                                  ("expectedInputs", TextBuilder.toText (TextBuilder.decimal fields.count))
                                ]
                            }
          ExceptT fields.binaryDecoder,
      textualEncoder = \_value -> error "TODO",
      textualDecoder = Atto.char '(' *> fields.textualDecoder <* Atto.char ')'
    }

compositeFieldRawText :: Atto.Parser (Maybe Text)
compositeFieldRawText = do
  raw <- go (0 :: Int) False False []
  pure (if Text.null raw then Nothing else Just raw)
  where
    go depth insideQuotes escaped accumulator = do
      nextChar <- Atto.peekChar
      case nextChar of
        Nothing -> fail "Unexpected end of input while decoding composite field"
        Just char
          | not insideQuotes && not escaped && depth == 0 && (char == ',' || char == ')') ->
              pure (Text.pack (reverse accumulator))
          | otherwise -> do
              _ <- Atto.anyChar
              if
                | escaped ->
                    go depth insideQuotes False (char : accumulator)
                | char == '\\' ->
                    go depth insideQuotes True (char : accumulator)
                | char == '"' ->
                    go depth (not insideQuotes) False (char : accumulator)
                | not insideQuotes && char == '(' ->
                    go (depth + 1) insideQuotes False (char : accumulator)
                | not insideQuotes && char == ')' && depth > 0 ->
                    go (depth - 1) insideQuotes False (char : accumulator)
                | otherwise ->
                    go depth insideQuotes False (char : accumulator)

decodeQuotedCompositeFieldText :: Text -> Either String Text
decodeQuotedCompositeFieldText text
  | Text.length text >= 2 && Text.head text == '"' && Text.last text == '"' =
      unescapeQuotedCompositeFieldText (Text.init (Text.tail text))
  | otherwise =
      Right text

unescapeQuotedCompositeFieldText :: Text -> Either String Text
unescapeQuotedCompositeFieldText =
  go False []
  where
    go escaped accumulator input =
      case Text.uncons input of
        Nothing ->
          if escaped
            then Left "Composite field has trailing escape character"
            else Right (Text.pack (reverse accumulator))
        Just (char, rest)
          | escaped ->
              go False (char : accumulator) rest
          | char == '\\' ->
              go True accumulator rest
          | otherwise ->
              go False (char : accumulator) rest
