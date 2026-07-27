module IntegrationTest.ScalarSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Pool as Pool
import qualified Data.Set as Set
import Data.Tagged (Tagged (..))
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Integrations.Pq (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Algebra as Algebra
import qualified PostgresqlTypes.Bool
import qualified PostgresqlTypes.Bytea
import qualified PostgresqlTypes.Float4
import qualified PostgresqlTypes.Float8
import qualified PostgresqlTypes.Int2
import qualified PostgresqlTypes.Int4
import qualified PostgresqlTypes.Int8
import qualified PostgresqlTypes.Uuid
import qualified PtrPeeker
import Test.Hspec
import Prelude

-- ---------------------------------------------------------------------------
-- Enum type for testing

data Genre = Comedy | Action | Drama
  deriving (Eq, Ord, Show)

genreScalar :: PostgresqlCodecs.Scalar Genre
genreScalar =
  PostgresqlCodecs.enum
    "public"
    "movie_genre"
    [("Comedy", Comedy), ("Action", Action), ("Drama", Drama)]

-- ---------------------------------------------------------------------------
-- IsPrimitive instance declaring a non-default schema, for testing that
-- 'PostgresqlCodecs.binaryPrimitive' honors it instead of hardcoding "".

data SchemaedWidget

instance Algebra.IsPrimitive SchemaedWidget where
  schemaName = Tagged (Just "custom_schema")
  typeName = Tagged "widget"
  textualEncoder = error "not exercised by this test"
  textualDecoder = error "not exercised by this test"

instance Algebra.IsBinaryPrimitive SchemaedWidget where
  binaryEncoder = error "not exercised by this test"
  binaryDecoder = error "not exercised by this test"

-- ---------------------------------------------------------------------------
-- IsPrimitive instance with no IsBinaryPrimitive instance, for testing that
-- 'PostgresqlCodecs.textualPrimitive' declines binary support.

data TextOnlyWidget = TextOnlyWidget

instance Algebra.IsPrimitive TextOnlyWidget where
  schemaName = Tagged Nothing
  typeName = Tagged "text_only_widget"
  textualEncoder = error "not exercised by this test"
  textualDecoder = error "not exercised by this test"

spec :: SpecHook.HookedSpec
spec = do
  describe "Scalar" $ do
    describe "binaryPrimitive unresolvedTypes" $ do
      it "is empty when both OIDs are statically known" $ \_env ->
        (PostgresqlCodecs.binaryPrimitive @PostgresqlTypes.Int4).unresolvedTypes `shouldBe` mempty

      it "contains the type itself when the OIDs are not statically known" $ \_env ->
        (PostgresqlCodecs.binaryPrimitive @PostgresqlTypes.Citext).unresolvedTypes
          `shouldBe` Set.singleton ("", "citext")

      it "qualifies the unresolved type with the instance's declared schema" $ \_env ->
        (PostgresqlCodecs.binaryPrimitive @SchemaedWidget).unresolvedTypes
          `shouldBe` Set.singleton ("custom_schema", "widget")

    describe "binaryPrimitive schemaName" $ do
      it "is empty when the instance declares no schema" $ \_env ->
        (PostgresqlCodecs.binaryPrimitive @PostgresqlTypes.Int4).schemaName `shouldBe` ""

      it "matches the instance's declared schema" $ \_env ->
        (PostgresqlCodecs.binaryPrimitive @SchemaedWidget).schemaName `shouldBe` "custom_schema"

    describe "textualPrimitive" $ do
      it "reports supportsBinary as False" $ \_env ->
        (PostgresqlCodecs.textualPrimitive @TextOnlyWidget).supportsBinary `shouldBe` False

      it "rejects binary encoding" $ \_env -> do
        let scalar = PostgresqlCodecs.textualPrimitive @TextOnlyWidget
        case scalar.binaryEncoder (\_ _ -> PostgresqlCodecs.TypeOids 0 0) TextOnlyWidget of
          Left _ -> pure ()
          Right _ -> expectationFailure "expected binary encoding to be rejected"

      it "rejects binary decoding" $ \_env -> do
        let scalar = PostgresqlCodecs.textualPrimitive @TextOnlyWidget
            result =
              PtrPeeker.runVariableOnByteString
                (scalar.binaryDecoder (\_ _ -> PostgresqlCodecs.TypeOids 0 0))
                BS.empty
        case result of
          Right (Left _) -> pure ()
          _ -> expectationFailure "expected binary decoding to be rejected"

    describe "custom" $ do
      it "passes supportsBinary through" $ \_env -> do
        let base = PostgresqlCodecs.binaryPrimitive @PostgresqlTypes.Int4
            scalar =
              PostgresqlCodecs.custom
                base.schemaName
                base.typeName
                base.baseOid
                base.arrayOid
                False
                base.binaryEncoder
                base.binaryDecoder
                base.textualEncoder
                base.textualDecoder
        scalar.supportsBinary `shouldBe` False

      it "derives unresolvedTypes as empty when both OIDs are given" $ \_env -> do
        let base = PostgresqlCodecs.binaryPrimitive @PostgresqlTypes.Int4
            scalar =
              PostgresqlCodecs.custom
                base.schemaName
                base.typeName
                base.baseOid
                base.arrayOid
                True
                base.binaryEncoder
                base.binaryDecoder
                base.textualEncoder
                base.textualDecoder
        scalar.unresolvedTypes `shouldBe` mempty

      it "derives unresolvedTypes as the type itself when either OID is missing" $ \_env -> do
        let base = PostgresqlCodecs.binaryPrimitive @PostgresqlTypes.Int4
            scalar =
              PostgresqlCodecs.custom
                "public"
                "widget"
                Nothing
                base.arrayOid
                True
                base.binaryEncoder
                base.binaryDecoder
                base.textualEncoder
                base.textualDecoder
        scalar.unresolvedTypes `shouldBe` Set.singleton ("public", "widget")

    describe "binaryPrimitive roundtrips" $ do
      it "int2" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.binaryPrimitive
            (PostgresqlTypes.Int2.fromInt16 42)
            "int2"
        result `shouldBe` Right (PostgresqlTypes.Int2.fromInt16 42)

      it "int4" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.binaryPrimitive
            (PostgresqlTypes.Int4.fromInt32 42)
            "int4"
        result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)

      it "int8" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.binaryPrimitive
            (PostgresqlTypes.Int8.fromInt64 9876543210)
            "int8"
        result `shouldBe` Right (PostgresqlTypes.Int8.fromInt64 9876543210)

      it "float4" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.binaryPrimitive
            (PostgresqlTypes.Float4.fromFloat 3.14)
            "float4"
        result `shouldBe` Right (PostgresqlTypes.Float4.fromFloat 3.14)

      it "float8" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.binaryPrimitive
            (PostgresqlTypes.Float8.fromDouble 3.14)
            "float8"
        result `shouldBe` Right (PostgresqlTypes.Float8.fromDouble 3.14)

      it "text" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.binaryPrimitive
            ("hello" :: PostgresqlTypes.Text)
            "text"
        result `shouldBe` Right ("hello" :: PostgresqlTypes.Text)

      it "bool" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.binaryPrimitive
            (PostgresqlTypes.Bool.fromBool True)
            "bool"
        result `shouldBe` Right (PostgresqlTypes.Bool.fromBool True)

      it "bytea" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.binaryPrimitive
            (PostgresqlTypes.Bytea.fromByteString "hello")
            "bytea"
        result `shouldBe` Right (PostgresqlTypes.Bytea.fromByteString "hello")

      it "uuid" $ \env -> do
        let val = PostgresqlTypes.Uuid.fromUUID (read "550e8400-e29b-41d4-a716-446655440000")
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.binaryPrimitive
            val
            "uuid"
        result `shouldBe` Right val

    describe "enum" $ do
      it "roundtrip" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          execQuery
            conn
            "SELECT $1::movie_genre"
            (PostgresqlCodecs.paramWith genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.columnWith genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            env.binary
            Comedy
        result `shouldBe` Right Comedy

-- ---------------------------------------------------------------------------
-- Helper

scalarRoundtrip :: (Eq a, Show a) => PQ.Connection -> Bool -> PostgresqlCodecs.Scalar a -> a -> BS.ByteString -> IO (Either PostgresqlCodecs.Report a)
scalarRoundtrip conn isBinary scalar val pgType =
  execQuery
    conn
    ("SELECT $1::" <> pgType)
    (PostgresqlCodecs.paramWith scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
    (PostgresqlCodecs.oneRow (PostgresqlCodecs.columnWith scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
    isBinary
    val
