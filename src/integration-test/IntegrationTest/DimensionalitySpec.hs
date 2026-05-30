module IntegrationTest.DimensionalitySpec (spec) where

import qualified Data.Pool as Pool
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import IntegrationTest.Adapter (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import Test.Hspec
import Prelude

spec :: SpecHook.HookedSpec
spec = do
  describe "Dimensionality" $ do
    it "d1 non-nullable array roundtrip" $ \env -> do
      if not env.binary
        then pendingWith "Text array encoding/decoding in Dimensionality is TODO"
        else pure ()
      let input = Vector.fromList [PostgresqlTypes.Int4.fromInt32 1, PostgresqlTypes.Int4.fromInt32 2, PostgresqlTypes.Int4.fromInt32 3]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d1 nullable elements array roundtrip" $ \env -> do
      if not env.binary
        then pendingWith "Text array encoding/decoding in Dimensionality is TODO"
        else pure ()
      let input = Vector.fromList [Just (PostgresqlTypes.Int4.fromInt32 1), Nothing, Just (PostgresqlTypes.Int4.fromInt32 3)]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d1 empty array roundtrip" $ \env -> do
      pendingWith "PostgreSQL encodes empty arrays with dimension count 0, which the library rejects"
      let input = Vector.fromList [] :: Vector PostgresqlTypes.Int4
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d1 single element array roundtrip" $ \env -> do
      if not env.binary
        then pendingWith "Text array encoding/decoding in Dimensionality is TODO"
        else pure ()
      let input = Vector.fromList [PostgresqlTypes.Int4.fromInt32 42]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input
