{-# LANGUAGE OverloadedRecordDot #-}

module IntegrationTest.Adapter
  ( execQuery,
  )
where

import Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgresqlCodecs
import qualified PostgresqlCodecs.Report as Report
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder
import Prelude

-- | A libpq-based row decoder that threads through column index state.
newtype RowDecoder a
  = RowDecoder
      (PQ.Result -> PQ.Row -> IORef Int -> IO (Either Report.Report a))

instance Functor RowDecoder where
  fmap f (RowDecoder run) = RowDecoder $ \res row colRef ->
    fmap (fmap f) (run res row colRef)

instance Applicative RowDecoder where
  pure x = RowDecoder $ \_ _ _ -> pure (Right x)
  RowDecoder rf <*> RowDecoder rx = RowDecoder $ \res row colRef -> do
    ef <- rf res row colRef
    case ef of
      Left e -> pure (Left e)
      Right f -> fmap (fmap f) (rx res row colRef)

instance PostgresqlCodecs.ConsumesColumns RowDecoder where
  consumeColumn onNull peeker _parser = RowDecoder $ \res row colRef -> do
    colIdx <- readIORef colRef
    modifyIORef' colRef (+ 1)
    mBytes <- PQ.getvalue' res row (PQ.Col (fromIntegral colIdx))
    case mBytes of
      Nothing -> pure onNull
      Just bytes ->
        case PtrPeeker.runVariableOnByteString peeker bytes of
          Left offset ->
            pure $
              Left
                Report.Report
                  { message = "Binary decoding failed",
                    suggestion = "",
                    details =
                      [ ("details", "Ran out of input"),
                        ("row", fromString (show row)),
                        ("column", fromString (show colIdx)),
                        ("inputOffset", fromString (show offset))
                      ]
                  }
          Right result -> pure result

-- | Decode a single row from a libpq result, advancing through columns.
decodeRow :: PostgresqlCodecs.Columns row -> PQ.Result -> PQ.Row -> IO (Either Report.Report row)
decodeRow PostgresqlCodecs.Columns {decoder = rd} pgRes pgRow = do
  colRef <- newIORef 0
  let RowDecoder run = rd
  run pgRes pgRow colRef

-- | Execute a parameterised query and decode the result.
execQuery ::
  PQ.Connection ->
  BS.ByteString ->
  PostgresqlCodecs.Params params ->
  PostgresqlCodecs.Result result ->
  Bool ->
  params ->
  IO (Either Report.Report result)
execQuery conn sql ps result isBinaryEncodingAllowed input = do
  case parameterEncoder input of
    Left err -> pure (Left err)
    Right encodedValues -> do
      let paramList =
            zipWith
              ( \oid mEncodedValue ->
                  fmap (\encodedValue -> (PQ.Oid (fromIntegral oid), encodedValue, parameterFormat)) mEncodedValue
              )
              ps.oids
              encodedValues
      mPgResult <- PQ.execParams conn sql paramList PQ.Binary
      case mPgResult of
        Nothing -> do
          mErr <- PQ.errorMessage conn
          pure $ Left $ maybe (fromString "execParams returned Nothing") (fromString . Text.unpack . Text.Encoding.decodeUtf8) mErr
        Just pgResult -> do
          status <- PQ.resultStatus pgResult
          handleResult pgResult status result
  where
    handleResult pgResult status theResult = case theResult of
      PostgresqlCodecs.RowsAffected toResult ->
        case status of
          PQ.CommandOk -> do
            mBS <- PQ.cmdTuples pgResult
            let countStr = maybe "0" id mBS
            case BS8.readInt countStr of
              Just (n, _) -> pure (toResult n)
              Nothing ->
                pure (Left (fromString ("Invalid row count: " <> Text.unpack (Text.Encoding.decodeUtf8 countStr))))
          PQ.TuplesOk -> do
            nrows <- PQ.ntuples pgResult
            let PQ.Row nCI = nrows
            pure (toResult (fromIntegral nCI))
          _ -> queryError pgResult
      PostgresqlCodecs.MaybeRow cols noRow toRow ->
        case status of
          PQ.TuplesOk -> do
            nrows <- PQ.ntuples pgResult
            if nrows == 0
              then pure (Right noRow)
              else fmap (fmap toRow) (decodeRow cols pgResult (PQ.Row 0))
          _ -> queryError pgResult
      PostgresqlCodecs.OneRow cols ->
        case status of
          PQ.TuplesOk -> do
            nrows <- PQ.ntuples pgResult
            if nrows /= 1
              then
                pure $
                  Left $
                    fromString
                      ( "Expected 1 row, got "
                          <> show (let PQ.Row nCI = nrows in fromIntegral nCI :: Int)
                      )
              else decodeRow cols pgResult (PQ.Row 0)
          _ -> queryError pgResult
      PostgresqlCodecs.ManyRows cols initAcc foldRow fin ->
        case status of
          PQ.TuplesOk -> do
            nrows <- PQ.ntuples pgResult
            let rows = [PQ.Row 0 .. nrows - 1]
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

    queryError pgResult = do
      mErr <- PQ.resultErrorMessage pgResult
      pure $ Left $ maybe (fromString "Query failed") (fromString . Text.unpack . Text.Encoding.decodeUtf8) mErr
    parameterEncoder =
      if isBinaryEncodingAllowed
        then fmap (fmap (fmap Write.toByteString)) . ps.binaryEncoder
        else fmap (fmap (fmap (Text.Encoding.encodeUtf8 . TextBuilder.toText))) . ps.textualEncoder
    parameterFormat =
      if isBinaryEncodingAllowed
        then PQ.Binary
        else PQ.Text
