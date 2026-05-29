module IntegrationTest.ParamsSpec (spec) where

import Data.Functor.Contravariant
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Adapter (execQuery)
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Bool
import qualified PostgresqlTypes.Float8
import qualified PostgresqlTypes.Int4
import qualified PostgresqlTypes.Int8
import Test.Hspec
import Prelude

spec :: SpecWith (Pool.Pool PQ.Connection)
spec = aroundWith (\action pool -> Pool.withResource pool action) $ do
  describe "Params" $ do
    it "single int4 param" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT $1::int4"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          True
          (PostgresqlTypes.Int4.fromInt32 42)
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)

    it "single int8 param" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT $1::int8"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          True
          (PostgresqlTypes.Int8.fromInt64 9876543210)
      result `shouldBe` Right (PostgresqlTypes.Int8.fromInt64 9876543210)

    it "single text param" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT $1::text"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          True
          ("hello" :: PostgresqlTypes.Text)
      result `shouldBe` Right ("hello" :: PostgresqlTypes.Text)

    it "single bool param" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT $1::bool"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          True
          (PostgresqlTypes.Bool.fromBool True)
      result `shouldBe` Right (PostgresqlTypes.Bool.fromBool True)

    it "single float8 param" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT $1::float8"
          (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          True
          (PostgresqlTypes.Float8.fromDouble 3.14)
      result `shouldBe` Right (PostgresqlTypes.Float8.fromDouble 3.14)

    it "multiple params" $ \conn -> do
      let ps =
            contramap fst (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
              <> contramap snd (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          cols =
            (,)
              <$> PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
      result <-
        execQuery
          conn
          "SELECT $1::int4, $2::int4"
          ps
          (PostgresqlCodecs.oneRow cols)
          True
          (PostgresqlTypes.Int4.fromInt32 10, PostgresqlTypes.Int4.fromInt32 20)
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 10, PostgresqlTypes.Int4.fromInt32 20)
