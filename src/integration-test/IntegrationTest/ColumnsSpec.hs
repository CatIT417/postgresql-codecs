module IntegrationTest.ColumnsSpec (spec) where

import qualified Data.Pool as Pool
import Data.Vector (Vector)
import Data.Word (Word32)
import IntegrationTest.Integrations.Pq (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import qualified PostgresqlTypes.Int8
import Test.Hspec
import Prelude

-- ---------------------------------------------------------------------------
-- Enum type for testing the resolved column OID.
-- Only used in pure tests, so it needs no server-side counterpart.

data Color = Red | Green deriving (Eq, Ord, Show)

colorScalar :: PostgresqlCodecs.Scalar Color
colorScalar = PostgresqlCodecs.enum "" "column_color" [("red", Red), ("green", Green)]

spec :: SpecHook.HookedSpec
spec = do
  describe "Columns" $ do
    it "oids resolves the declared column type" $ \_env -> do
      let sentinelBaseOid = 424242 :: Word32
          sentinelArrayOid = 424243 :: Word32
          resolver s n =
            if (s, n) == ("", "column_color")
              then PostgresqlCodecs.TypeOids sentinelBaseOid sentinelArrayOid
              else PostgresqlCodecs.TypeOids 0 0
          zeroResolver _ _ = PostgresqlCodecs.TypeOids 0 0
          scalarColumn =
            PostgresqlCodecs.columnWith colorScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
          arrayColumn :: PostgresqlCodecs.Columns (Vector Color)
          arrayColumn =
            PostgresqlCodecs.columnWith
              colorScalar
              (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector)
              PostgresqlCodecs.nonNullable
      scalarColumn.oids resolver `shouldBe` [sentinelBaseOid]
      scalarColumn.oids zeroResolver `shouldBe` [0]
      arrayColumn.oids resolver `shouldBe` [sentinelArrayOid]
      arrayColumn.oids zeroResolver `shouldBe` [0]

    it "single int4 column" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 42::int4"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          ()
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)

    it "multiple columns" $ \env -> do
      let cols =
            (,)
              <$> PostgresqlCodecs.column PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
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
              <$> PostgresqlCodecs.column PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
              <*> PostgresqlCodecs.column PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT 42::int4, 9876543210::int8, 'hello'::text"
          mempty
          (PostgresqlCodecs.oneRow cols)
          env.binary
          ()
      result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42, PostgresqlTypes.Int8.fromInt64 9876543210, "hello" :: PostgresqlTypes.Text)
