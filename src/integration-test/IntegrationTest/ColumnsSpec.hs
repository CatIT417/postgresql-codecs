module IntegrationTest.ColumnsSpec (spec) where

import qualified Data.Pool as Pool
import IntegrationTest.Integrations.Pq (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import qualified PostgresqlTypes.Int8
import Test.Hspec
import Prelude

spec :: SpecHook.HookedSpec
spec = do
  describe "Columns" $ do
    it "single int4 column" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 42::int4"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          ()
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)

    it "multiple columns" $ \env -> do
      let cols =
            (,)
              <$> PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 10::int4, 20::int4"
          mempty
          (PostgresqlCodecs.oneRow cols)
          env.binary
          ()
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 10, PostgresqlTypes.Int4.fromInt32 20)

    it "mixed type columns" $ \env -> do
      let cols =
            (,,)
              <$> PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.standard PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 42::int4, 9876543210::int8, 'hello'::text"
          mempty
          (PostgresqlCodecs.oneRow cols)
          env.binary
          ()
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42, PostgresqlTypes.Int8.fromInt64 9876543210, "hello" :: PostgresqlTypes.Text)
