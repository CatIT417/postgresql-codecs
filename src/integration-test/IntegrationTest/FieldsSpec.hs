module IntegrationTest.FieldsSpec (spec) where

import qualified Data.Pool as Pool
import Data.Profunctor
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Demo.Types (Artist (..), Genre (..), artist)
import IntegrationTest.Integrations.Pq (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
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
        <$> lmap (.ptX) (PostgresqlCodecs.field PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
        <*> lmap (.ptY) (PostgresqlCodecs.field PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
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
        <*> lmap (.containerLabel) (PostgresqlCodecs.field PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
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
        <$> lmap (.cnX) (PostgresqlCodecs.field PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
        <*> lmap (.cnY) (PostgresqlCodecs.field PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
    )

newtype CompositeWithArray = CompositeWithArray {cwaXs :: Vector PostgresqlTypes.Int4}
  deriving (Eq, Show)

compositeWithArrayScalar :: PostgresqlCodecs.Scalar CompositeWithArray
compositeWithArrayScalar =
  PostgresqlCodecs.composite
    "public"
    "composite_with_array"
    ( CompositeWithArray
        <$> lmap
          (.cwaXs)
          ( PostgresqlCodecs.field
              PostgresqlCodecs.binaryPrimitive
              (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector)
              PostgresqlCodecs.nonNullable
          )
    )

-- | Declares its first field as @float4@ while the real server-side
-- @composite_point@ type has @int4@ there — same byte width, different OID,
-- so a mismatch wouldn't fail on size alone.
data MismatchedPoint = MismatchedPoint {mpX :: PostgresqlTypes.Float4, mpY :: PostgresqlTypes.Int4}
  deriving (Eq, Show)

mismatchedPointScalar :: PostgresqlCodecs.Scalar MismatchedPoint
mismatchedPointScalar =
  PostgresqlCodecs.composite
    "public"
    "composite_point"
    ( MismatchedPoint
        <$> lmap (.mpX) (PostgresqlCodecs.field PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
        <*> lmap (.mpY) (PostgresqlCodecs.field PostgresqlCodecs.binaryPrimitive PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
    )

spec :: SpecHook.HookedSpec
spec = do
  describe "Fields" $ do
    it "composite roundtrip" $ \env -> do
      let pt = CompositePoint {ptX = PostgresqlTypes.Int4.fromInt32 42, ptY = PostgresqlTypes.Int4.fromInt32 7}
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::composite_point"
          (PostgresqlCodecs.param compositePointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column compositePointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          pt
      result `shouldBe` Right pt

    it "composite with nullable field non-null" $ \env -> do
      let val = CompositeWithNullable {cnX = PostgresqlTypes.Int4.fromInt32 1, cnY = Just (PostgresqlTypes.Int4.fromInt32 2)}
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::composite_with_nullable"
          (PostgresqlCodecs.param compositeWithNullableScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column compositeWithNullableScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          val
      result `shouldBe` Right val

    it "composite with nullable field null" $ \env -> do
      let val = CompositeWithNullable {cnX = PostgresqlTypes.Int4.fromInt32 1, cnY = Nothing}
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::composite_with_nullable"
          (PostgresqlCodecs.param compositeWithNullableScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column compositeWithNullableScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          val
      result `shouldBe` Right val

    it "composite with enum field binary roundtrip (Artist)" $ \env -> do
      let val = Artist {name = "Miles", genre = Jazz}
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::artist"
          (PostgresqlCodecs.param artist PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column artist PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          val
      result `shouldBe` Right val

    it "composite with array field roundtrip" $ \env -> do
      let val =
            CompositeWithArray
              { cwaXs =
                  Vector.fromList
                    [ PostgresqlTypes.Int4.fromInt32 1,
                      PostgresqlTypes.Int4.fromInt32 2,
                      PostgresqlTypes.Int4.fromInt32 3
                    ]
              }
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT $1::composite_with_array"
          (PostgresqlCodecs.param compositeWithArrayScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column compositeWithArrayScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          val
      result `shouldBe` Right val

    it "composite field OID mismatch is a hard decode error" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT ROW(42, 7)::composite_point"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column mismatchedPointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          ()
      case result of
        Left _ -> pure ()
        Right value -> expectationFailure ("expected a field-OID mismatch error, got: " <> show value)

    it "nested composite decode" $ \env -> do
      result <- Pool.withResource env.pool $ \conn ->
        execQuery
          conn
          "SELECT ROW(ROW(42, 7)::composite_point, 'hello, world')::composite_container"
          mempty
          (PostgresqlCodecs.oneRow (PostgresqlCodecs.column compositeContainerScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
          env.binary
          ()
      result
        `shouldBe` Right
          CompositeContainer
            { containerPoint = CompositePoint {ptX = PostgresqlTypes.Int4.fromInt32 42, ptY = PostgresqlTypes.Int4.fromInt32 7},
              containerLabel = "hello, world"
            }
