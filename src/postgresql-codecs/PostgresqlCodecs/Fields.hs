module PostgresqlCodecs.Fields where

import qualified Data.Attoparsec.Text as Atto
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import PostgresqlCodecs.Core (Report (..), Resolver, Scalar (..))
import PostgresqlCodecs.Dimensionality
import PostgresqlCodecs.IsScalar
import PostgresqlCodecs.Nullability
import PostgresqlCodecs.Prelude
import PostgresqlCodecs.Scalar (declaredOid, elementOid)
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Codec for the fields of a PostgreSQL composite type.
-- @a@ is the input type for encoding; @b@ is the output type for decoding.
data Fields a b = Fields
  { count :: Int32,
    -- | Transitive set of @(schema, name)@ pairs of referenced types whose base
    -- or array OID is not statically known and therefore must be resolved at runtime.
    unresolvedTypes :: Set (Text, Text),
    -- | Whether every nested field's scalar supports binary encoding.
    supportsBinary :: Bool,
    binaryEncoder :: Resolver -> a -> Either Report Write.Write,
    binaryDecoder :: Resolver -> PtrPeeker.Variable (Either Report b),
    -- | Represent in Postgres textual format.
    textualEncoder :: Resolver -> a -> Either Report [TextBuilder.TextBuilder],
    textualDecoder :: Resolver -> Atto.Parser b
  }

instance Functor (Fields a) where
  fmap f Fields {..} =
    Fields
      { count,
        unresolvedTypes,
        supportsBinary,
        binaryEncoder,
        binaryDecoder = \r -> fmap (fmap f) (binaryDecoder r),
        textualEncoder,
        textualDecoder = \r -> fmap f (textualDecoder r)
      }

instance Applicative (Fields a) where
  pure x =
    Fields
      { count = 0,
        unresolvedTypes = mempty,
        supportsBinary = True,
        binaryEncoder = \_resolver -> const (Right mempty),
        binaryDecoder = \_resolver -> pure (Right x),
        textualEncoder = \_resolver -> const (Right []),
        textualDecoder = \_resolver -> pure x
      }
  (<*>) left right =
    Fields
      { count = left.count + right.count,
        unresolvedTypes = left.unresolvedTypes <> right.unresolvedTypes,
        supportsBinary = left.supportsBinary && right.supportsBinary,
        binaryEncoder = \resolver value -> liftA2 (<>) (left.binaryEncoder resolver value) (right.binaryEncoder resolver value),
        binaryDecoder = \resolver -> liftA2 (<*>) (left.binaryDecoder resolver) (right.binaryDecoder resolver),
        textualEncoder = \resolver value -> do
          leftTexts <- left.textualEncoder resolver value
          rightTexts <- right.textualEncoder resolver value
          Right (leftTexts <> rightTexts),
        textualDecoder = \resolver ->
          if left.count == 0 || right.count == 0
            then left.textualDecoder resolver <*> right.textualDecoder resolver
            else left.textualDecoder resolver <*> (Atto.char ',' *> right.textualDecoder resolver)
      }

instance Profunctor Fields where
  dimap f g Fields {..} =
    Fields
      { count,
        unresolvedTypes,
        supportsBinary,
        binaryEncoder = \resolver value -> binaryEncoder resolver (f value),
        binaryDecoder = \r -> fmap (fmap g) (binaryDecoder r),
        textualEncoder = \resolver value -> textualEncoder resolver (f value),
        textualDecoder = \r -> fmap g (textualDecoder r)
      }

-- | Report for when a composite field's wire OID contradicts its resolved, statically or
-- runtime-known declared OID.
fieldOidMismatchReport :: Word32 -> Int32 -> Report
fieldOidMismatchReport expectedOid actualOid =
  Report
    { message = "Field OID mismatch",
      suggestion = "",
      details =
        [ ("details", "Expected OID " <> TextBuilder.toText (TextBuilder.decimal expectedOid) <> ", but got " <> TextBuilder.toText (TextBuilder.decimal (fromIntegral actualOid :: Word32))),
          ("inputLocation", "[field-oid]"),
          ("inputOffset", "0"),
          ("failingInput", TextBuilder.toText (TextBuilder.decimal (fromIntegral actualOid :: Word32))),
          ("expectedInputs", TextBuilder.toText (TextBuilder.decimal expectedOid))
        ]
    }

-- | Build a single composite field codec from @a@'s 'IsScalar' instance, a dimensionality, and
-- a nullability.
field :: (IsScalar a) => Dimensionality a b -> Nullability b c -> Fields c c
field = fieldWith scalar

-- | Build a single composite field codec from an explicit scalar, dimensionality, and
-- nullability.
--
-- Reach for this only when @a@ has no fixed static representation to hang an 'IsScalar'
-- instance on - e.g. a codec resolved at runtime from schema introspection. Never use it to
-- give a type that already has (or should have) an 'IsScalar' instance a different codec at
-- some call site; that reintroduces the coupling 'IsScalar' exists to prevent. A type needing
-- more than one representation should be a distinct newtype with its own instance instead.
fieldWith :: Scalar a -> Dimensionality a b -> Nullability b c -> Fields c c
fieldWith scalar dimensionality nullability =
  let Nullability {encoder = enc} = nullability
   in Fields
        { count = 1,
          unresolvedTypes = scalar.unresolvedTypes,
          supportsBinary = scalar.supportsBinary,
          binaryEncoder = \resolver value ->
            -- The field's own type slot holds the DECLARED type of the field - the array
            -- type when the field is array-typed. The array header nested inside the
            -- payload holds the ELEMENT type. Conflating the two makes the server reject
            -- the whole record.
            let binEnc = dimensionality.binaryEncoder (elementOid scalar resolver) (scalar.binaryEncoder resolver)
                oid = Write.bInt32 (fromIntegral (declaredOid scalar dimensionality.dims resolver))
             in enc
                  (Right (oid <> Write.bInt32 (-1)))
                  ( \a -> do
                      write <- binEnc a
                      let size = Write.bInt32 (fromIntegral (Write.writeSize write))
                      Right (oid <> size <> write)
                  )
                  value,
          binaryDecoder = \resolver -> do
            fieldOid <- PtrPeeker.fixed PtrPeeker.beSignedInt4
            contentSize <- PtrPeeker.fixed PtrPeeker.beSignedInt4
            let expectedOid = declaredOid scalar dimensionality.dims resolver
            -- A resolver that couldn't determine the type reports 0, which is not a real
            -- OID to contradict - trust the wire in that case, same as before this check
            -- existed. Only a POSITIVE mismatch (both sides known, and disagreeing) is a
            -- real error; re-validating would otherwise hard-fail on every resolver miss.
            if expectedOid /= 0 && fromIntegral fieldOid /= expectedOid
              then do
                case contentSize of
                  -1 -> pure ()
                  _ -> void (PtrPeeker.forceSize (fromIntegral contentSize) (pure ()))
                pure (Left (fieldOidMismatchReport expectedOid fieldOid))
              else case contentSize of
                -1 -> pure nullability.onNull
                _ ->
                  fmap
                    (fmap nullability.onNonNull)
                    (PtrPeeker.forceSize (fromIntegral contentSize) (dimensionality.binaryDecoder (elementOid scalar resolver) (scalar.binaryDecoder resolver))),
          textualEncoder = \resolver value ->
            let txtEnc = dimensionality.textualEncoder (scalar.textualEncoder resolver)
             in enc
                  (Right [mempty])
                  (\a -> do text <- txtEnc a; Right [text])
                  value,
          textualDecoder = \resolver -> do
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
                case Atto.parseOnly (dimensionality.textualDecoder (scalar.textualDecoder resolver) <* Atto.endOfInput) normalizedFieldText of
                  Left message -> fail message
                  Right value -> pure (nullability.onNonNull value)
        }

-- | Build a 'Scalar' for a PostgreSQL composite type from a 'Fields' specification.
composite :: Text -> Text -> Fields a a -> Scalar a
composite schemaName typeName fields =
  Scalar
    { schemaName,
      typeName,
      baseOid = Nothing,
      arrayOid = Nothing,
      supportsBinary = fields.supportsBinary,
      unresolvedTypes = Set.insert (schemaName, typeName) fields.unresolvedTypes,
      binaryEncoder = \resolver value -> do
        fieldWrites <- fields.binaryEncoder resolver value
        Right (Write.bInt32 fields.count <> fieldWrites),
      binaryDecoder = \resolver ->
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
          ExceptT (fields.binaryDecoder resolver),
      textualEncoder = \resolver value -> do
        fieldTexts <- fields.textualEncoder resolver value
        let renderedFields = map renderCompositeField fieldTexts
        Right
          ( TextBuilder.text "("
              <> mconcat (intersperse (TextBuilder.text ",") renderedFields)
              <> TextBuilder.text ")"
          ),
      textualDecoder = \resolver -> Atto.char '(' *> fields.textualDecoder resolver <* Atto.char ')'
    }

renderCompositeField :: TextBuilder -> TextBuilder
renderCompositeField tb =
  let t = TextBuilder.toText tb
   in if Text.null t
        then mempty
        else
          if Text.any compositeSpecialChar t
            then
              TextBuilder.text "\""
                <> TextBuilder.text (Text.concatMap escapeCompositeChar t)
                <> TextBuilder.text "\""
            else tb
  where
    compositeSpecialChar c = c == ',' || c == '(' || c == ')' || c == '"' || c == '\\' || isSpace c
    escapeCompositeChar '"' = "\\\""
    escapeCompositeChar '\\' = "\\\\"
    escapeCompositeChar c = Text.singleton c

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
