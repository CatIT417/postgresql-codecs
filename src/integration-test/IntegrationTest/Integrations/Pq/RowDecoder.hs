module IntegrationTest.Integrations.Pq.RowDecoder
  ( RowDecoder,
    run,
  )
where

import Data.String (fromString)
import qualified Database.PostgreSQL.LibPQ as Pq
import qualified PostgresqlCodecs
import qualified PtrPeeker
import Prelude

-- | A libpq-based row decoder that threads through column index state.
newtype RowDecoder a
  = RowDecoder
      ( Pq.Result ->
        Pq.Row ->
        Pq.Column ->
        IO (Either PostgresqlCodecs.Report (a, Pq.Column))
      )

run ::
  RowDecoder a ->
  Pq.Result ->
  Pq.Row ->
  Pq.Column ->
  IO (Either PostgresqlCodecs.Report (a, Pq.Column))
run (RowDecoder run) = run

instance Functor RowDecoder where
  fmap f (RowDecoder run) = RowDecoder $ \res row colRef ->
    fmap (fmap (\(a, col) -> (f a, col))) (run res row colRef)

instance Applicative RowDecoder where
  pure x = RowDecoder $ \_ _ col -> pure (Right (x, col))
  RowDecoder rf <*> RowDecoder rx = RowDecoder $ \res row colRef -> do
    ef <- rf res row colRef
    case ef of
      Left e -> pure (Left e)
      Right (f, nextCol) ->
        fmap (fmap (\(x, finalCol) -> (f x, finalCol))) (rx res row nextCol)

instance PostgresqlCodecs.ConsumesColumns RowDecoder where
  consumeColumn onNull peeker _parser = RowDecoder $ \res row colRef -> do
    let Pq.Col colIdx = colRef
    let nextCol = Pq.Col (colIdx + 1)
    mBytes <- Pq.getvalue' res row colRef
    case mBytes of
      Nothing -> pure (fmap (\x -> (x, nextCol)) onNull)
      Just bytes ->
        case PtrPeeker.runVariableOnByteString peeker bytes of
          Left offset ->
            pure $
              Left
                PostgresqlCodecs.Report
                  { message = "Binary decoding failed",
                    suggestion = "",
                    details =
                      [ ("details", "Ran out of input"),
                        ("row", fromString (show row)),
                        ("column", fromString (show colIdx)),
                        ("inputOffset", fromString (show offset))
                      ]
                  }
          Right result -> pure (fmap (\x -> (x, nextCol)) result)
