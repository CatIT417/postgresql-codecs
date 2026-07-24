module IntegrationTest.ScalarSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Pool as Pool
import qualified Data.Set as Set
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Integrations.Pq (execQuery)
import qualified IntegrationTest.SpecHook as SpecHook
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Bool
import qualified PostgresqlTypes.Bytea
import qualified PostgresqlTypes.Float4
import qualified PostgresqlTypes.Float8
import qualified PostgresqlTypes.Int2
import qualified PostgresqlTypes.Int4
import qualified PostgresqlTypes.Int8
import qualified PostgresqlTypes.Uuid
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

spec :: SpecHook.HookedSpec
spec = do
  describe "Scalar" $ do
    describe "standard unresolvedTypes" $ do
      it "is empty when both OIDs are statically known" $ \_env ->
        (PostgresqlCodecs.standard @PostgresqlTypes.Int4).unresolvedTypes `shouldBe` mempty

      it "contains the type itself when the OIDs are not statically known" $ \_env ->
        (PostgresqlCodecs.standard @PostgresqlTypes.Citext).unresolvedTypes
          `shouldBe` Set.singleton ("", "citext")

    describe "standard roundtrips" $ do
      it "int2" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.standard
            (PostgresqlTypes.Int2.fromInt16 42)
            "int2"
        result `shouldBe` Right (PostgresqlTypes.Int2.fromInt16 42)

      it "int4" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.standard
            (PostgresqlTypes.Int4.fromInt32 42)
            "int4"
        result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)

      it "int8" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.standard
            (PostgresqlTypes.Int8.fromInt64 9876543210)
            "int8"
        result `shouldBe` Right (PostgresqlTypes.Int8.fromInt64 9876543210)

      it "float4" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.standard
            (PostgresqlTypes.Float4.fromFloat 3.14)
            "float4"
        result `shouldBe` Right (PostgresqlTypes.Float4.fromFloat 3.14)

      it "float8" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.standard
            (PostgresqlTypes.Float8.fromDouble 3.14)
            "float8"
        result `shouldBe` Right (PostgresqlTypes.Float8.fromDouble 3.14)

      it "text" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.standard
            ("hello" :: PostgresqlTypes.Text)
            "text"
        result `shouldBe` Right ("hello" :: PostgresqlTypes.Text)

      it "bool" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.standard
            (PostgresqlTypes.Bool.fromBool True)
            "bool"
        result `shouldBe` Right (PostgresqlTypes.Bool.fromBool True)

      it "bytea" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.standard
            (PostgresqlTypes.Bytea.fromByteString "hello")
            "bytea"
        result `shouldBe` Right (PostgresqlTypes.Bytea.fromByteString "hello")

      it "uuid" $ \env -> do
        let val = PostgresqlTypes.Uuid.fromUUID (read "550e8400-e29b-41d4-a716-446655440000")
        result <- Pool.withResource env.pool $ \conn ->
          scalarRoundtrip
            conn
            env.binary
            PostgresqlCodecs.standard
            val
            "uuid"
        result `shouldBe` Right val

    describe "enum" $ do
      it "roundtrip" $ \env -> do
        result <- Pool.withResource env.pool $ \conn ->
          execQuery
            conn
            "SELECT $1::movie_genre"
            (PostgresqlCodecs.param genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
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
    (PostgresqlCodecs.param scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
    (PostgresqlCodecs.oneRow (PostgresqlCodecs.column scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
    isBinary
    val
