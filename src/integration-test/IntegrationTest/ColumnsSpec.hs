module IntegrationTest.ColumnsSpec (spec) where

import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Prelude
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import qualified PostgresqlTypes.Int8
import Test.Hspec
import Prelude

spec :: SpecWith PQ.Connection
spec = do
  describe "Columns" $ do
    it "single int4 column" $ \conn -> do
      result <-
        query
          conn
          "SELECT 42::int4"
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)

    it "multiple columns" $ \conn -> do
      let cols =
            (,)
              <$> PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
      result <-
        query
          conn
          "SELECT 10::int4, 20::int4"
          (PostgresqlCodecs.oneRow cols)
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 10, PostgresqlTypes.Int4.fromInt32 20)

    it "mixed type columns" $ \conn -> do
      let cols =
            (,,)
              <$> PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
      result <-
        query
          conn
          "SELECT 42::int4, 9876543210::int8, 'hello'::text"
          (PostgresqlCodecs.oneRow cols)
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42, PostgresqlTypes.Int8.fromInt64 9876543210, "hello" :: PostgresqlTypes.Text)
