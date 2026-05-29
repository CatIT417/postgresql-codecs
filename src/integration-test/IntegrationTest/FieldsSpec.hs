{-# LANGUAGE OverloadedRecordDot #-}

module IntegrationTest.FieldsSpec (spec) where

import Data.Profunctor
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Adapter (execQuery)
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Int4
import Test.Hspec
import Prelude

-- ---------------------------------------------------------------------------
-- Composite types from shared test schema

data CompositePoint = CompositePoint {ptX :: PostgresqlTypes.Int4, ptY :: PostgresqlTypes.Int4}
  deriving (Eq, Show)

compositePointScalar :: PostgresqlCodecs.Scalar CompositePoint
compositePointScalar =
  PostgresqlCodecs.composite
    "public"
    "composite_point"
    ( CompositePoint
        <$> lmap (.ptX) (PostgresqlCodecs.field PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
        <*> lmap (.ptY) (PostgresqlCodecs.field PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
    )

data CompositeContainer = CompositeContainer {containerPoint :: CompositePoint, containerLabel :: PostgresqlTypes.Text}
  deriving (Eq, Show)

compositeContainerScalar :: PostgresqlCodecs.Scalar CompositeContainer
compositeContainerScalar =
  PostgresqlCodecs.composite
    "public"
    "composite_container"
    ( CompositeContainer
        <$> lmap (.containerPoint) (PostgresqlCodecs.field compositePointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
        <*> lmap (.containerLabel) (PostgresqlCodecs.field PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
    )

data CompositeWithNullable = CompositeWithNullable
  { cnX :: PostgresqlTypes.Int4,
    cnY :: Maybe PostgresqlTypes.Int4
  }
  deriving (Eq, Show)

compositeWithNullableScalar :: PostgresqlCodecs.Scalar CompositeWithNullable
compositeWithNullableScalar =
  PostgresqlCodecs.composite
    "public"
    "composite_with_nullable"
    ( CompositeWithNullable
        <$> lmap (.cnX) (PostgresqlCodecs.field PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
        <*> lmap (.cnY) (PostgresqlCodecs.field PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
    )

setupTypes :: PQ.Connection -> IO PQ.Connection
setupTypes conn = do
  _ <- PQ.exec conn "DROP TYPE IF EXISTS composite_with_nullable CASCADE"
  _ <- PQ.exec conn "CREATE TYPE composite_with_nullable AS (x int4, y int4)"
  pure conn

spec :: SpecWith PQ.Connection
spec = beforeAllWith setupTypes $ do
  describe "Fields" $ do
    it "composite roundtrip" $ \conn -> do
      pendingWith "PostgresqlCodecs.Fields.composite textual codec is TODO"
      let pt = CompositePoint {ptX = PostgresqlTypes.Int4.fromInt32 42, ptY = PostgresqlTypes.Int4.fromInt32 7}
      result <-
        execQuery
          conn
          "SELECT $1::composite_point"
          (PostgresqlCodecs.param compositePointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column compositePointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          pt
      result `shouldBe` Right pt

    it "composite with nullable field non-null" $ \conn -> do
      pendingWith "PostgresqlCodecs.Fields.composite textual codec is TODO"
      let val = CompositeWithNullable {cnX = PostgresqlTypes.Int4.fromInt32 1, cnY = Just (PostgresqlTypes.Int4.fromInt32 2)}
      result <-
        execQuery
          conn
          "SELECT $1::composite_with_nullable"
          (PostgresqlCodecs.param compositeWithNullableScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column compositeWithNullableScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          val
      result `shouldBe` Right val

    it "composite with nullable field null" $ \conn -> do
      pendingWith "PostgresqlCodecs.Fields.composite textual codec is TODO"
      let val = CompositeWithNullable {cnX = PostgresqlTypes.Int4.fromInt32 1, cnY = Nothing}
      result <-
        execQuery
          conn
          "SELECT $1::composite_with_nullable"
          (PostgresqlCodecs.param compositeWithNullableScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column compositeWithNullableScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          val
      result `shouldBe` Right val

    it "nested composite decode" $ \conn -> do
      result <-
        execQuery
          conn
          "SELECT ROW(ROW(42, 7)::composite_point, 'hello, world')::composite_container"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column compositeContainerScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          ()
      result
        `shouldBe` Right
          CompositeContainer
            { containerPoint = CompositePoint {ptX = PostgresqlTypes.Int4.fromInt32 42, ptY = PostgresqlTypes.Int4.fromInt32 7},
              containerLabel = "hello, world"
            }
