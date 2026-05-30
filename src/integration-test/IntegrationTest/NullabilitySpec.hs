module IntegrationTest.NullabilitySpec (spec) where

import qualified Data.Pool as Pool
import IntegrationTest.Adapter (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import Test.Hspec
import Prelude

spec :: SpecHook.HookedSpec
spec = do
  describe "Nullability" $ do
    it "nullable column with non-null value" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 42::int4"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          env.binary
          ()
      result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 42))

    it "nullable column with null" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT NULL::int4"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          env.binary
          ()
      result `shouldBe` Right (Nothing :: Maybe PostgresqlTypes.Int4)

    it "non-nullable column rejects null" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT NULL::int4"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable) :: PostgresqlCodecs.Result PostgresqlTypes.Int4)
          env.binary
          ()
      result `shouldBe` Left ("Unexpected null" :: PostgresqlCodecs.Report)

    it "nullable param with Nothing" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          env.binary
          (Nothing :: Maybe PostgresqlTypes.Int4)
      result `shouldBe` Right (Nothing :: Maybe PostgresqlTypes.Int4)

    it "nullable param with Just" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          env.binary
          (Just (PostgresqlTypes.Int4.fromInt32 42))
      result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 42))

    it "nullable param roundtrip through nullable column" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          env.binary
          (Just (PostgresqlTypes.Int4.fromInt32 99))
      result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 99))
