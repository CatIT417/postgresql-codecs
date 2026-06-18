module IntegrationTest.ResultSpec (spec) where

import qualified Data.Pool as Pool
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Integrations.Pq (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import Test.Hspec
import Prelude

spec :: SpecHook.HookedSpec
spec = do
  describe "Result" $ do
    it "rowsAffected on INSERT" $ \env -> do
      result <- Pool.withResource env.pool $ \conn -> do
        _ <- PQ.exec conn "CREATE TEMP TABLE t_affected (x int4)"
        execQuery
          conn
          "INSERT INTO t_affected SELECT generate_series(1, 5)"
          mempty
          PostgresqlCodecs.rowsAffected
          env.binary
          ()
      result `shouldBe` Right 5

    it "rowsAffected on SELECT" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT * FROM unnest(ARRAY[1,2,3]::int4[])"
          mempty
          PostgresqlCodecs.rowsAffected
          env.binary
          ()
      result `shouldBe` Right 3

    it "oneRow returns the row" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 42::int4"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          ()
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)

    it "oneRow errors on no rows" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 42::int4 WHERE false"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable) :: PostgresqlCodecs.Result PostgresqlTypes.Int4)
          env.binary
          ()
      result `shouldBe` Left ("Expected 1 row, got 0" :: PostgresqlCodecs.Report)

    it "oneRow errors on too many rows" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT * FROM unnest(ARRAY[1,2]::int4[])"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable) :: PostgresqlCodecs.Result PostgresqlTypes.Int4)
          env.binary
          ()
      result `shouldBe` Left ("Expected 1 row, got 2" :: PostgresqlCodecs.Report)

    it "oneRow errors on column count mismatch" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 1::int4, 2::int4"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable) :: PostgresqlCodecs.Result PostgresqlTypes.Int4)
          env.binary
          ()
      result
        `shouldBe` Left
          PostgresqlCodecs.Report
            { message = "Unexpected number of columns in result set",
              suggestion = "Ensure the selected columns match the Columns decoder.",
              details =
                [ ("expectedColumns", "1"),
                  ("actualColumns", "2")
                ]
            }

    it "someRow validates column count even with zero rows" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 1::int4, 2::int4 WHERE false"
          mempty
          (PostgresqlCodecs.someRow (PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable) :: PostgresqlCodecs.Result (Maybe PostgresqlTypes.Int4))
          env.binary
          ()
      result
        `shouldBe` Left
          PostgresqlCodecs.Report
            { message = "Unexpected number of columns in result set",
              suggestion = "Ensure the selected columns match the Columns decoder.",
              details =
                [ ("expectedColumns", "1"),
                  ("actualColumns", "2")
                ]
            }

    it "someRow returns Just when present" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 42::int4"
          mempty
          (PostgresqlCodecs.someRow (PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          ()
      result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 42))

    it "someRow returns Nothing when absent" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 42::int4 WHERE false"
          mempty
          (PostgresqlCodecs.someRow (PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          ()
      result `shouldBe` Right (Nothing :: Maybe PostgresqlTypes.Int4)

    it "manyRows returns multiple rows" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT * FROM unnest(ARRAY[1,2,3]::int4[])"
          mempty
          (PostgresqlCodecs.manyRows (PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          ()
      result `shouldBe` Right (Vector.fromList [PostgresqlTypes.Int4.fromInt32 1, PostgresqlTypes.Int4.fromInt32 2, PostgresqlTypes.Int4.fromInt32 3])

    it "manyRows returns empty vector" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT * FROM unnest(ARRAY[]::int4[])"
          mempty
          (PostgresqlCodecs.manyRows (PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          ()
      result `shouldBe` Right (Vector.fromList [] :: Vector PostgresqlTypes.Int4)
