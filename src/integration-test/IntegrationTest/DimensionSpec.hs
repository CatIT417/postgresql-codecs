module IntegrationTest.DimensionSpec (spec) where

import qualified Data.Pool as Pool
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Adapter (execQuery)
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Bool
import qualified PostgresqlTypes.Int8
import Test.Hspec
import Prelude

spec :: SpecWith (Pool.Pool PQ.Connection)
spec = aroundWith (\action pool -> Pool.withResource pool action) $ do
  describe "Dimension" $ do
    it "vector of int8" $ \conn -> do
      let input = Vector.fromList [PostgresqlTypes.Int8.fromInt64 1, PostgresqlTypes.Int8.fromInt64 2]
      result <-
        execQuery
          conn
          "SELECT $1::int8[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          True
          input
      result `shouldBe` Right input

    it "vector of text" $ \conn -> do
      pendingWith "Text array binary decoding reads beyond element boundary"
      let input = Vector.fromList ["hello" :: PostgresqlTypes.Text, "world"]
      result <-
        execQuery
          conn
          "SELECT $1::text[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          True
          input
      result `shouldBe` Right input

    it "vector of bool" $ \conn -> do
      let input = Vector.fromList [PostgresqlTypes.Bool.fromBool True, PostgresqlTypes.Bool.fromBool False, PostgresqlTypes.Bool.fromBool True]
      result <-
        execQuery
          conn
          "SELECT $1::bool[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          True
          input
      result `shouldBe` Right input
