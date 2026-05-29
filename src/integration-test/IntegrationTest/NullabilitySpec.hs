module IntegrationTest.NullabilitySpec (spec) where

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Adapter (execQuery)
import qualified PostgresqlCodecs
import qualified PostgresqlCodecs.DecodingError as DecodingError
import qualified PostgresqlCodecs.Report as Report
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import Test.Hspec
import Prelude

spec :: SpecWith (Pool.Pool PQ.Connection)
spec = aroundWith (\action pool -> Pool.withResource pool action) $ do
  describe "Nullability" $ do
    it "nullable column with non-null value" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT 42::int4"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          True
          ()
      result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 42))

    it "nullable column with null" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT NULL::int4"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          True
          ()
      result `shouldBe` Right (Nothing :: Maybe PostgresqlTypes.Int4)

    it "non-nullable column rejects null" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT NULL::int4"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable) :: PostgresqlCodecs.Result PostgresqlTypes.Int4)
          True
          ()
      result
        `shouldBe` Left (Report.fromModel (("Unexpected null" :: DecodingError.DecodingError)))

    it "nullable param with Nothing" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT $1::int4"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          True
          (Nothing :: Maybe PostgresqlTypes.Int4)
      result `shouldBe` Right (Nothing :: Maybe PostgresqlTypes.Int4)

    it "nullable param with Just" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT $1::int4"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          True
          (Just (PostgresqlTypes.Int4.fromInt32 42))
      result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 42))

    it "nullable param roundtrip through nullable column" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT $1::int4"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
          True
          (Just (PostgresqlTypes.Int4.fromInt32 99))
      result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 99))
