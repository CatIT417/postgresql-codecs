module IntegrationTest.DimensionalitySpec (spec) where

import qualified Data.Pool as Pool
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Adapter (execQuery)
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import Test.Hspec
import Prelude

spec :: SpecWith (Pool.Pool PQ.Connection)
spec = aroundWith (\action pool -> Pool.withResource pool action) $ do
  describe "Dimensionality" $ do
    it "d1 non-nullable array roundtrip" $ \conn -> do
      let input = Vector.fromList [PostgresqlTypes.Int4.fromInt32 1, PostgresqlTypes.Int4.fromInt32 2, PostgresqlTypes.Int4.fromInt32 3]
      result <-
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          input
      result `shouldBe` Right input

    it "d1 nullable elements array roundtrip" $ \conn -> do
      let input = Vector.fromList [Just (PostgresqlTypes.Int4.fromInt32 1), Nothing, Just (PostgresqlTypes.Int4.fromInt32 3)]
      result <-
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          input
      result `shouldBe` Right input

    it "d1 empty array roundtrip" $ \conn -> do
      pendingWith "PostgreSQL encodes empty arrays with dimension count 0, which the library rejects"
      let input = Vector.fromList [] :: Vector PostgresqlTypes.Int4
      result <-
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          input
      result `shouldBe` Right input

    it "d1 single element array roundtrip" $ \conn -> do
      let input = Vector.fromList [PostgresqlTypes.Int4.fromInt32 42]
      result <-
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          input
      result `shouldBe` Right input
