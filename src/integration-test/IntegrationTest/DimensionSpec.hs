module IntegrationTest.DimensionSpec (spec) where

import qualified Data.Pool as Pool
import qualified Data.Vector as Vector
import IntegrationTest.Integrations.Pq (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Bool
import qualified PostgresqlTypes.Int8
import Test.Hspec
import Prelude

spec :: SpecHook.HookedSpec
spec = do
  describe "Dimension" $ do
    it "vector of int8" $ \env -> do
      let input = Vector.fromList [PostgresqlTypes.Int8.fromInt64 1, PostgresqlTypes.Int8.fromInt64 2]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int8[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "vector of text" $ \env -> do
      pendingWith "Text array binary decoding reads beyond element boundary"
      let input = Vector.fromList ["hello" :: PostgresqlTypes.Text, "world"]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::text[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "vector of bool" $ \env -> do
      let input = Vector.fromList [PostgresqlTypes.Bool.fromBool True, PostgresqlTypes.Bool.fromBool False, PostgresqlTypes.Bool.fromBool True]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::bool[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input
