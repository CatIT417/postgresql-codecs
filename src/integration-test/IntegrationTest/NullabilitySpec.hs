module IntegrationTest.NullabilitySpec (spec) where

import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Adapter (query, queryParams)
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import Test.Hspec
import Prelude

spec :: SpecWith PQ.Connection
spec = do
  describe "Nullability" $ do
    it "nullable column with non-null value" $ \conn -> do
      result <-
        query
          conn
          "SELECT 42::int4"
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
      result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 42))

    it "nullable column with null" $ \conn -> do
      result <-
        query
          conn
          "SELECT NULL::int4"
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
      result `shouldBe` Right (Nothing :: Maybe PostgresqlTypes.Int4)

    it "non-nullable column rejects null" $ \conn -> do
      result <-
        query
          conn
          "SELECT NULL::int4"
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable) :: PostgresqlCodecs.Result PostgresqlTypes.Int4)
      result `shouldBe` Left "Unexpected null"

    it "nullable param with Nothing" $ \conn -> do
      result <-
        queryParams
          conn
          "SELECT $1::int4"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          (Nothing :: Maybe PostgresqlTypes.Int4)
      result `shouldBe` Right (Nothing :: Maybe PostgresqlTypes.Int4)

    it "nullable param with Just" $ \conn -> do
      result <-
        queryParams
          conn
          "SELECT $1::int4"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          (Just (PostgresqlTypes.Int4.fromInt32 42))
      result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 42))

    it "nullable param roundtrip through nullable column" $ \conn -> do
      result <-
        queryParams
          conn
          "SELECT $1::int4"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          (Just (PostgresqlTypes.Int4.fromInt32 99))
      result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 99))
