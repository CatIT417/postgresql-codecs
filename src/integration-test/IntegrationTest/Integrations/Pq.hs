module IntegrationTest.Integrations.Pq
  ( execQuery,
  )
where

import Control.Monad (foldM, forM)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.List (zipWith4)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Read as Text.Read
import Data.Word (Word32)
import qualified Database.PostgreSQL.LibPQ as Pq
import qualified IntegrationTest.Integrations.Pq.RowDecoder as RowDecoder
import qualified PostgresqlCodecs.Core as PostgresqlCodecs
import Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Build a real 'PostgresqlCodecs.Resolver' by looking the referenced types up
-- in the live catalog. An empty set skips the query entirely; anything the
-- catalog cannot resolve defaults to @TypeOids 0 0@.
buildResolver :: Pq.Connection -> Set (Text.Text, Text.Text) -> IO PostgresqlCodecs.Resolver
buildResolver conn unresolved
  | Set.null unresolved = pure (\_ _ -> PostgresqlCodecs.TypeOids 0 0)
  | otherwise = do
      let pairs = Set.toList unresolved
          schemas = map fst pairs
          names = map snd pairs
          sql =
            "SELECT q.schema, q.name, \
            \COALESCE(t.oid::text, '0'), COALESCE(t.typarray::text, '0') \
            \FROM unnest($1::text[], $2::text[]) AS q(schema, name) \
            \LEFT JOIN pg_type t ON t.oid = to_regtype( \
            \  CASE WHEN q.schema = '' THEN quote_ident(q.name) \
            \       ELSE quote_ident(q.schema) || '.' || quote_ident(q.name) END)::oid"
          params =
            [ Just (Pq.Oid 0, textArrayLiteral schemas, Pq.Text),
              Just (Pq.Oid 0, textArrayLiteral names, Pq.Text)
            ]
      mRes <- Pq.execParams conn sql params Pq.Text
      m <- case mRes of
        Just res -> do
          st <- Pq.resultStatus res
          case st of
            Pq.TuplesOk -> do
              Pq.Row n <- Pq.ntuples res
              rows <- forM [0 .. n - 1] $ \i -> do
                sc <- getText res i 0
                nm <- getText res i 1
                b <- getText res i 2
                a <- getText res i 3
                pure ((sc, nm), PostgresqlCodecs.TypeOids (readWord b) (readWord a))
              pure (Map.fromList rows)
            _ -> pure Map.empty
        Nothing -> pure Map.empty
      pure (\schema name -> Map.findWithDefault (PostgresqlCodecs.TypeOids 0 0) (schema, name) m)

-- | Render a PostgreSQL @text[]@ literal with every element double-quoted and
-- backslash\/quote escaped, so empty-schema and arbitrary identifiers are safe.
textArrayLiteral :: [Text.Text] -> ByteString.ByteString
textArrayLiteral elements =
  Text.Encoding.encodeUtf8
    ("{" <> Text.intercalate "," (map quote elements) <> "}")
  where
    quote element = "\"" <> Text.concatMap escape element <> "\""
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape c = Text.singleton c

-- | Read a text column value, decoding it as UTF-8. Columns are never NULL here
-- thanks to @COALESCE@ and the non-NULL input arrays.
getText :: (Integral n) => Pq.Result -> n -> n -> IO Text.Text
getText res i j = do
  mBytes <- Pq.getvalue' res (Pq.toRow i) (Pq.toColumn j)
  pure (maybe Text.empty Text.Encoding.decodeUtf8 mBytes)

-- | Parse a decimal 'Word32', treating unparseable input as 0.
readWord :: Text.Text -> Word32
readWord t = case Text.Read.decimal t of
  Right (n, _) -> n
  Left _ -> 0

-- | Decode a single row from a libpq result, advancing through columns.
decodeRow :: PostgresqlCodecs.Resolver -> PostgresqlCodecs.Columns row -> Pq.Result -> Pq.Row -> IO (Either PostgresqlCodecs.Report row)
decodeRow resolver PostgresqlCodecs.Columns {decoder} pgRes pgRow =
  fmap (fmap fst) (RowDecoder.run (decoder resolver) pgRes pgRow (Pq.Col 0))

-- | Execute a parameterised query and decode the result.
execQuery ::
  Pq.Connection ->
  ByteString.ByteString ->
  PostgresqlCodecs.Params params ->
  PostgresqlCodecs.Result result ->
  Bool ->
  params ->
  IO (Either PostgresqlCodecs.Report result)
execQuery conn sql ps result isBinaryEncodingAllowed input = do
  let unresolved = ps.unresolvedTypes <> PostgresqlCodecs.unresolvedTypes result
  resolver <- buildResolver conn unresolved
  case (,) <$> ps.binaryEncoder resolver input <*> ps.textualEncoder resolver input of
    Left err -> pure (Left err)
    Right (binaryValues, textualValues) -> do
      let paramList =
            zipWith4 selectParameter (ps.oids resolver) ps.formats binaryValues textualValues
      mPgResult <- Pq.execParams conn sql paramList Pq.Binary
      case mPgResult of
        Nothing -> do
          mErr <- Pq.errorMessage conn
          pure $ Left $ maybe (fromString "execParams returned Nothing") (fromString . Text.unpack . Text.Encoding.decodeUtf8) mErr
        Just pgResult -> do
          status <- Pq.resultStatus pgResult
          handleResult resolver pgResult status result
  where
    handleResult resolver pgResult status theResult = case theResult of
      PostgresqlCodecs.RowsAffected toResult ->
        case status of
          Pq.CommandOk -> do
            mBS <- Pq.cmdTuples pgResult
            let countStr = maybe "0" id mBS
            case ByteString.Char8.readInt countStr of
              Just (n, _) -> pure (toResult n)
              Nothing ->
                pure (Left (fromString ("Invalid row count: " <> Text.unpack (Text.Encoding.decodeUtf8 countStr))))
          Pq.TuplesOk -> do
            nrows <- Pq.ntuples pgResult
            let Pq.Row nCI = nrows
            pure (toResult (fromIntegral nCI))
          _ -> queryError pgResult
      PostgresqlCodecs.MaybeRow cols noRow toRow ->
        case status of
          Pq.TuplesOk ->
            withValidatedColumns cols pgResult do
              nrows <- Pq.ntuples pgResult
              if nrows == 0
                then pure noRow
                else fmap (>>= toRow) (decodeRow resolver cols pgResult (Pq.Row 0))
          _ -> queryError pgResult
      PostgresqlCodecs.OneRow cols toRow ->
        case status of
          Pq.TuplesOk ->
            withValidatedColumns cols pgResult do
              nrows <- Pq.ntuples pgResult
              if nrows /= 1
                then
                  pure
                    $ Left
                    $ fromString
                      ( "Expected 1 row, got "
                          <> show (let Pq.Row nCI = nrows in fromIntegral nCI :: Int)
                      )
                else fmap (>>= toRow) (decodeRow resolver cols pgResult (Pq.Row 0))
          _ -> queryError pgResult
      PostgresqlCodecs.ManyRows cols initAcc foldRow fin ->
        case status of
          Pq.TuplesOk ->
            withValidatedColumns cols pgResult do
              nrows <- Pq.ntuples pgResult
              let rows = [Pq.Row 0 .. nrows - 1]
              foldResult <-
                foldM
                  ( \accE rowIdx -> case accE of
                      Left e -> pure (Left e)
                      Right acc -> do
                        rowResult <- decodeRow resolver cols pgResult rowIdx
                        pure $ case rowResult of
                          Left e -> Left e
                          Right r -> foldRow acc r
                  )
                  (Right initAcc)
                  rows
              pure $ case foldResult of
                Left e -> Left e
                Right acc -> fin acc
          _ -> queryError pgResult

    withValidatedColumns cols pgResult action = do
      countValidation <- validateColumnCount cols pgResult
      case countValidation of
        Left report -> pure (Left report)
        Right () -> action

    validateColumnCount cols pgResult = do
      actualCount <- Pq.nfields pgResult
      let expectedCount = fromIntegral cols.count :: Pq.Column
      if actualCount == expectedCount
        then pure (Right ())
        else
          let Pq.Col actualCountInt = actualCount
              Pq.Col expectedCountInt = expectedCount
           in pure $
                Left
                  PostgresqlCodecs.Report
                    { message = "Unexpected number of columns in result set",
                      suggestion = "Ensure the selected columns match the Columns decoder.",
                      details =
                        [ ("expectedColumns", fromString (show expectedCountInt)),
                          ("actualColumns", fromString (show actualCountInt))
                        ]
                    }

    queryError pgResult = do
      mErr <- Pq.resultErrorMessage pgResult
      pure $ Left $ maybe (fromString "Query failed") (fromString . Text.unpack . Text.Encoding.decodeUtf8) mErr

    -- The global toggle can only downgrade a parameter to text: a parameter
    -- that declares it cannot do binary is never sent in the binary format.
    effectiveFormat declaredFormat =
      if not isBinaryEncodingAllowed
        then PostgresqlCodecs.Textual
        else declaredFormat

    -- Pick the encoding of one parameter. 'Nothing' in both encoded lists means
    -- a NULL parameter and stays 'Nothing'.
    selectParameter oid declaredFormat mBinaryValue mTextualValue =
      case effectiveFormat declaredFormat of
        PostgresqlCodecs.Binary ->
          fmap
            (\value -> (Pq.Oid (fromIntegral oid), Write.toByteString value, Pq.Binary))
            mBinaryValue
        PostgresqlCodecs.Textual ->
          fmap
            (\value -> (Pq.Oid (fromIntegral oid), Text.Encoding.encodeUtf8 (TextBuilder.toText value), Pq.Text))
            mTextualValue
