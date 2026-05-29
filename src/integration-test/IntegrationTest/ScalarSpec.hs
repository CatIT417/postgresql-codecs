module IntegrationTest.ScalarSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Database.PostgreSQL.LibPQ as PQ
import IntegrationTest.Adapter (execQuery)
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
    "genre"
    [("Comedy", Comedy), ("Action", Action), ("Drama", Drama)]

setupTypes :: PQ.Connection -> IO PQ.Connection
setupTypes conn = do
  _ <- PQ.exec conn "CREATE TYPE genre AS ENUM ('Comedy', 'Action', 'Drama')"
  pure conn

spec :: SpecWith PQ.Connection
spec = beforeAllWith setupTypes do
  describe "Scalar" $ do
    describe "native roundtrips" $ do
      it "int2" $ \conn -> do
        result <- scalarRoundtrip conn PostgresqlCodecs.native (PostgresqlTypes.Int2.fromInt16 42) "int2"
        result `shouldBe` Right (PostgresqlTypes.Int2.fromInt16 42)

      it "int4" $ \conn -> do
        result <- scalarRoundtrip conn PostgresqlCodecs.native (PostgresqlTypes.Int4.fromInt32 42) "int4"
        result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)

      it "int8" $ \conn -> do
        result <- scalarRoundtrip conn PostgresqlCodecs.native (PostgresqlTypes.Int8.fromInt64 9876543210) "int8"
        result `shouldBe` Right (PostgresqlTypes.Int8.fromInt64 9876543210)

      it "float4" $ \conn -> do
        result <- scalarRoundtrip conn PostgresqlCodecs.native (PostgresqlTypes.Float4.fromFloat 3.14) "float4"
        result `shouldBe` Right (PostgresqlTypes.Float4.fromFloat 3.14)

      it "float8" $ \conn -> do
        result <- scalarRoundtrip conn PostgresqlCodecs.native (PostgresqlTypes.Float8.fromDouble 3.14) "float8"
        result `shouldBe` Right (PostgresqlTypes.Float8.fromDouble 3.14)

      it "text" $ \conn -> do
        result <- scalarRoundtrip conn PostgresqlCodecs.native ("hello" :: PostgresqlTypes.Text) "text"
        result `shouldBe` Right ("hello" :: PostgresqlTypes.Text)

      it "bool" $ \conn -> do
        result <- scalarRoundtrip conn PostgresqlCodecs.native (PostgresqlTypes.Bool.fromBool True) "bool"
        result `shouldBe` Right (PostgresqlTypes.Bool.fromBool True)

      it "bytea" $ \conn -> do
        result <- scalarRoundtrip conn PostgresqlCodecs.native (PostgresqlTypes.Bytea.fromByteString "hello") "bytea"
        result `shouldBe` Right (PostgresqlTypes.Bytea.fromByteString "hello")

      it "uuid" $ \conn -> do
        let val = PostgresqlTypes.Uuid.fromUUID (read "550e8400-e29b-41d4-a716-446655440000")
        result <- scalarRoundtrip conn PostgresqlCodecs.native val "uuid"
        result `shouldBe` Right val

    describe "enum" $ do
      it "roundtrip" $ \conn -> do
        result <-
          execQuery
            conn
            "SELECT $1::genre"
            (PostgresqlCodecs.param genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            Comedy
        result `shouldBe` Right Comedy

-- ---------------------------------------------------------------------------
-- Helper

scalarRoundtrip :: (Eq a, Show a) => PQ.Connection -> PostgresqlCodecs.Scalar a -> a -> BS.ByteString -> IO (Either Text a)
scalarRoundtrip conn scalar val pgType =
  execQuery
    conn
    ("SELECT $1::" <> pgType)
    (PostgresqlCodecs.param scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
    (PostgresqlCodecs.oneRow (PostgresqlCodecs.column scalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
    val
