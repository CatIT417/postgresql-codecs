module IntegrationTest.Integrations.Pq
  ( execQuery,
  )
where

import Control.Monad (foldM)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Database.PostgreSQL.LibPQ as Pq
import qualified IntegrationTest.Integrations.Pq.RowDecoder as RowDecoder
import qualified PostgresqlCodecs
import qualified PtrPoker.Write as Write
import qualified TextBuilder
import Prelude

-- | Decode a single row from a libpq result, advancing through columns.
decodeRow :: PostgresqlCodecs.Columns row -> Pq.Result -> Pq.Row -> IO (Either PostgresqlCodecs.Report row)
decodeRow PostgresqlCodecs.Columns {decoder} pgRes pgRow =
  fmap (fmap fst) (RowDecoder.run decoder pgRes pgRow (Pq.Col 0))

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
  case parameterEncoder input of
    Left err -> pure (Left err)
    Right encodedValues -> do
      let paramList =
            zipWith
              ( \oid mEncodedValue ->
                  fmap (\encodedValue -> (Pq.Oid (fromIntegral oid), encodedValue, parameterFormat)) mEncodedValue
              )
              ps.oids
              encodedValues
      mPgResult <- Pq.execParams conn sql paramList Pq.Binary
      case mPgResult of
        Nothing -> do
          mErr <- Pq.errorMessage conn
          pure $ Left $ maybe (fromString "execParams returned Nothing") (fromString . Text.unpack . Text.Encoding.decodeUtf8) mErr
        Just pgResult -> do
          status <- Pq.resultStatus pgResult
          handleResult pgResult status result
  where
    handleResult pgResult status theResult = case theResult of
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
                then pure (Right noRow)
                else fmap (fmap toRow) (decodeRow cols pgResult (Pq.Row 0))
          _ -> queryError pgResult
      PostgresqlCodecs.OneRow cols ->
        case status of
          Pq.TuplesOk ->
            withValidatedColumns cols pgResult do
              nrows <- Pq.ntuples pgResult
              if nrows /= 1
                then
                  pure $
                    Left $
                      fromString
                        ( "Expected 1 row, got "
                            <> show (let Pq.Row nCI = nrows in fromIntegral nCI :: Int)
                        )
                else decodeRow cols pgResult (Pq.Row 0)
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
                        rowResult <- decodeRow cols pgResult rowIdx
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
    parameterEncoder =
      if isBinaryEncodingAllowed
        then fmap (fmap (fmap Write.toByteString)) . ps.binaryEncoder
        else fmap (fmap (fmap (Text.Encoding.encodeUtf8 . TextBuilder.toText))) . ps.textualEncoder
    parameterFormat =
      if isBinaryEncodingAllowed
        then Pq.Binary
        else Pq.Text
