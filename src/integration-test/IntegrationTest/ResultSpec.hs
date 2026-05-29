module IntegrationTest.ResultSpec (spec) where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Adapter (execQuery)
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import Test.Hspec
import Prelude

spec :: SpecWith PQ.Connection
spec = do
  describe "Result" $ do
    it "rowsAffected on INSERT" $ \conn -> do
      _ <- PQ.exec conn "CREATE TEMP TABLE t_affected (x int4)"
      result <-
        execQuery
          conn
          "INSERT INTO t_affected SELECT generate_series(1, 5)"
          mempty
          PostgresqlCodecs.rowsAffected
          ()
      result `shouldBe` Right 5

    it "rowsAffected on SELECT" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT * FROM unnest(ARRAY[1,2,3]::int4[])"
          mempty
          PostgresqlCodecs.rowsAffected
          ()
      result `shouldBe` Right 3

    it "oneRow returns the row" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT 42::int4"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          ()
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)

    it "oneRow errors on no rows" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT 42::int4 WHERE false"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable) :: PostgresqlCodecs.Result PostgresqlTypes.Int4)
          ()
      result `shouldBe` Left "Expected 1 row, got 0"

    it "oneRow errors on too many rows" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT * FROM unnest(ARRAY[1,2]::int4[])"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable) :: PostgresqlCodecs.Result PostgresqlTypes.Int4)
          ()
      result `shouldBe` Left "Expected 1 row, got 2"

    it "someRow returns Just when present" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT 42::int4"
          mempty
          (PostgresqlCodecs.someRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          ()
      result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 42))

    it "someRow returns Nothing when absent" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT 42::int4 WHERE false"
          mempty
          (PostgresqlCodecs.someRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          ()
      result `shouldBe` Right (Nothing :: Maybe PostgresqlTypes.Int4)

    it "manyRows returns multiple rows" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT * FROM unnest(ARRAY[1,2,3]::int4[])"
          mempty
          (PostgresqlCodecs.manyRows (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          ()
      result `shouldBe` Right (Vector.fromList [PostgresqlTypes.Int4.fromInt32 1, PostgresqlTypes.Int4.fromInt32 2, PostgresqlTypes.Int4.fromInt32 3])

    it "manyRows returns empty vector" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT * FROM unnest(ARRAY[]::int4[])"
          mempty
          (PostgresqlCodecs.manyRows (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          ()
      result `shouldBe` Right (Vector.fromList [] :: Vector PostgresqlTypes.Int4)
