module IntegrationTest.DimensionalitySpec (spec) where

import qualified Data.Pool as Pool
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Demo.Types
import IntegrationTest.Integrations.Pq (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import Test.Hspec
import Prelude

setupType :: SpecHook.Env -> IO SpecHook.Env
setupType env = do
  Pool.withResource env.pool $ \conn ->
    PQ.exec conn "CREATE TYPE genre AS ENUM ('rock', 'pop', 'jazz')"
  pure env

spec :: SpecHook.HookedSpec
spec = beforeAllWith setupType $ do
  describe "Dimensionality" $ do
    it "d1 non-nullable array roundtrip" $ \env -> do
      let input = Vector.fromList [PostgresqlTypes.Int4.fromInt32 1, PostgresqlTypes.Int4.fromInt32 2, PostgresqlTypes.Int4.fromInt32 3]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.standard (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d2 non-nullable array roundtrip" $ \env -> do
      let input =
            Vector.fromList
              [ Vector.fromList [PostgresqlTypes.Int4.fromInt32 1, PostgresqlTypes.Int4.fromInt32 2],
                Vector.fromList [PostgresqlTypes.Int4.fromInt32 3, PostgresqlTypes.Int4.fromInt32 4]
              ]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[][]"
          (PostgresqlCodecs.param PostgresqlCodecs.standard (PostgresqlCodecs.d2 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard (PostgresqlCodecs.d2 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d3 non-nullable array roundtrip" $ \env -> do
      let input =
            Vector.fromList
              [ Vector.fromList [Vector.fromList [PostgresqlTypes.Int4.fromInt32 1, PostgresqlTypes.Int4.fromInt32 2]],
                Vector.fromList [Vector.fromList [PostgresqlTypes.Int4.fromInt32 3, PostgresqlTypes.Int4.fromInt32 4]]
              ]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[][][]"
          (PostgresqlCodecs.param PostgresqlCodecs.standard (PostgresqlCodecs.d3 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard (PostgresqlCodecs.d3 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d4 non-nullable array roundtrip" $ \env -> do
      let input =
            Vector.fromList
              [ Vector.fromList [Vector.fromList [Vector.fromList [PostgresqlTypes.Int4.fromInt32 9, PostgresqlTypes.Int4.fromInt32 10]]]
              ]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[][][][]"
          (PostgresqlCodecs.param PostgresqlCodecs.standard (PostgresqlCodecs.d4 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard (PostgresqlCodecs.d4 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d5 non-nullable array roundtrip" $ \env -> do
      let input =
            Vector.fromList
              [ Vector.fromList [Vector.fromList [Vector.fromList [Vector.fromList [PostgresqlTypes.Int4.fromInt32 11, PostgresqlTypes.Int4.fromInt32 12]]]]
              ]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[][][][][]"
          (PostgresqlCodecs.param PostgresqlCodecs.standard (PostgresqlCodecs.d5 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard (PostgresqlCodecs.d5 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d6 non-nullable array roundtrip" $ \env -> do
      let input =
            Vector.fromList
              [ Vector.fromList [Vector.fromList [Vector.fromList [Vector.fromList [Vector.fromList [PostgresqlTypes.Int4.fromInt32 13, PostgresqlTypes.Int4.fromInt32 14]]]]]
              ]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[][][][][][]"
          (PostgresqlCodecs.param PostgresqlCodecs.standard (PostgresqlCodecs.d6 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard (PostgresqlCodecs.d6 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d1 nullable elements array roundtrip" $ \env -> do
      let input = Vector.fromList [Just (PostgresqlTypes.Int4.fromInt32 1), Nothing, Just (PostgresqlTypes.Int4.fromInt32 3)]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.standard (PostgresqlCodecs.d1 PostgresqlCodecs.nullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard (PostgresqlCodecs.d1 PostgresqlCodecs.nullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d1 empty array roundtrip" $ \env -> do
      let input = Vector.fromList [] :: Vector PostgresqlTypes.Int4
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.standard (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d1 single element array roundtrip" $ \env -> do
      let input = Vector.fromList [PostgresqlTypes.Int4.fromInt32 42]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::int4[]"
          (PostgresqlCodecs.param PostgresqlCodecs.standard (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.standard (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input

    it "d1 binary array roundtrip of enum" $ \env -> do
      let input = Vector.fromList [Demo.Types.Rock, Demo.Types.Jazz, Demo.Types.Pop]
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::genre[]"
          (PostgresqlCodecs.param Demo.Types.genre (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column Demo.Types.genre (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
          env.binary
          input
      result `shouldBe` Right input
