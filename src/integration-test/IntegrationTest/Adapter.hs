{-# LANGUAGE OverloadedRecordDot #-}

module IntegrationTest.Adapter
  ( execQuery,
  )
where

import Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgresqlCodecs
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import Prelude

-- | A libpq-based row decoder that threads through column index state.
newtype RowDecoder a
  = RowDecoder
      (PQ.Result -> PQ.Row -> IORef Int -> IO (Either PostgresqlCodecs.DecodingError a))

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
                PostgresqlCodecs.DecodingError
                  { message = "Binary decoding failed",
                    details = "Ran out of input at byte offset " <> Text.pack (show offset),
                    row = Nothing,
                    column = Nothing,
                    inputLocation = ["column-binary"],
                    inputOffset = fromIntegral offset,
                    failingInput = "",
                    expectedInputs = Nothing
                  }
          Right result -> pure result

-- | Decode a single row from a libpq result, advancing through columns.
decodeRow :: PostgresqlCodecs.Columns row -> PQ.Result -> PQ.Row -> IO (Either Text row)
decodeRow PostgresqlCodecs.Columns {decoder = rd} pgRes pgRow = do
  colRef <- newIORef 0
  let RowDecoder run = rd
  result <- run pgRes pgRow colRef
  pure $ case result of
    Left err -> Left err.message
    Right r -> Right r

-- | Execute a parameterised query and decode the result.
execQuery ::
  PQ.Connection ->
  BS.ByteString ->
  PostgresqlCodecs.Params params ->
  PostgresqlCodecs.Result result ->
  params ->
  IO (Either Text result)
execQuery conn sql ps result input = do
  case ps.binaryEncoder input of
    Left err -> pure (Left (Text.pack (show err)))
    Right writes -> do
      let paramList =
            zipWith
              ( \oid mWrite ->
                  fmap (\w -> (PQ.Oid (fromIntegral oid), Write.toByteString w, PQ.Binary)) mWrite
              )
              ps.oids
              writes
      mPgResult <- PQ.execParams conn sql paramList PQ.Binary
      case mPgResult of
        Nothing -> do
          mErr <- PQ.errorMessage conn
          pure $ Left $ maybe "execParams returned Nothing" Text.Encoding.decodeUtf8 mErr
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
                pure (Left ("Invalid row count: " <> Text.Encoding.decodeUtf8 countStr))
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
                    "Expected 1 row, got " <> Text.pack (show (let PQ.Row nCI = nrows in fromIntegral nCI :: Int))
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
      pure $ Left $ maybe "Query failed" Text.Encoding.decodeUtf8 mErr
