module IntegrationTest.Spec (spec) where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Contravariant
import Data.Profunctor
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified IntegrationTest.Adapter as Adapter
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Bool
import qualified PostgresqlTypes.Float8
import qualified PostgresqlTypes.Int4
import qualified PostgresqlTypes.Int8
import Test.Hspec
import qualified TestContainers.Hspec as TestContainers
import qualified TestContainers.Monad as TestContainers
import qualified TestcontainersPostgresql as TcPg
import Prelude

-- ---------------------------------------------------------------------------
-- Custom scalar types for testing

data Genre = Comedy | Action | Drama
  deriving (Eq, Ord, Show)

genreScalar :: PostgresqlCodecs.Scalar Genre
genreScalar =
  PostgresqlCodecs.enum
    "public"
    "genre"
    [("Comedy", Comedy), ("Action", Action), ("Drama", Drama)]

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

-- ---------------------------------------------------------------------------
-- Container setup

pgConfig :: TcPg.Config
pgConfig = TcPg.Config {tagName = "postgres:16", auth = TcPg.TrustAuth, forwardLogs = False}

setupContainer :: TestContainers.TestContainer PQ.Connection
setupContainer = do
  (host, port) <- TcPg.setup pgConfig
  let connStr =
        "host="
          <> BS8.pack (Text.unpack host)
          <> " port="
          <> BS8.pack (show port)
          <> " user=postgres dbname=postgres"
  conn <- liftIO (PQ.connectdb connStr)
  liftIO $ do
    s <- PQ.status conn
    case s of
      PQ.ConnectionOk -> pure ()
      _ -> fail "Failed to connect to PostgreSQL"
    _ <- PQ.exec conn "CREATE TYPE genre AS ENUM ('Comedy', 'Action', 'Drama')"
    _ <- PQ.exec conn "CREATE TYPE composite_point AS (x int4, y int4)"
    pure ()
  pure conn

-- ---------------------------------------------------------------------------
-- Test spec

spec :: Spec
spec =
  aroundAll (TestContainers.withContainers setupContainer) $ do
    describe "Integration" $ do
      -- Result kinds
      it "rowsAffected" $ \conn -> do
        _ <- PQ.exec conn "CREATE TEMP TABLE t_affected (x int4)"
        result <-
          Adapter.execQuery
            conn
            "INSERT INTO t_affected SELECT generate_series(1, 5)"
            mempty
            PostgresqlCodecs.rowsAffected
            ()
        result `shouldBe` Right 5
      it "oneRow int4" $ \conn -> do
        result <-
          Adapter.execQuery conn "SELECT 42::int4" mempty (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)) ()
        result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)
      it "someRow with result" $ \conn -> do
        result <-
          Adapter.execQuery conn "SELECT 42::int4" mempty (PostgresqlCodecs.someRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)) ()
        result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 42))
      it "someRow without result" $ \conn -> do
        result <-
          Adapter.execQuery
            conn
            "SELECT 42::int4 WHERE false"
            mempty
            (PostgresqlCodecs.someRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            ()
        result `shouldBe` Right (Nothing :: Maybe PostgresqlTypes.Int4)
      it "manyRows" $ \conn -> do
        result <-
          Adapter.execQuery
            conn
            "SELECT * FROM unnest(ARRAY[1,2,3]::int4[])"
            mempty
            (PostgresqlCodecs.manyRows (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            ()
        result `shouldBe` Right (Vector.fromList [PostgresqlTypes.Int4.fromInt32 1, PostgresqlTypes.Int4.fromInt32 2, PostgresqlTypes.Int4.fromInt32 3])
      -- Scalar parameter roundtrips
      it "param int4 roundtrip" $ \conn -> do
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::int4"
            (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            (PostgresqlTypes.Int4.fromInt32 42)
        result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 42)
      it "param int8 roundtrip" $ \conn -> do
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::int8"
            (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            (PostgresqlTypes.Int8.fromInt64 9876543210)
        result `shouldBe` Right (PostgresqlTypes.Int8.fromInt64 9876543210)
      it "param text roundtrip" $ \conn -> do
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::text"
            (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            ("hello" :: PostgresqlTypes.Text)
        result `shouldBe` Right ("hello" :: PostgresqlTypes.Text)
      it "param bool roundtrip" $ \conn -> do
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::bool"
            (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            (PostgresqlTypes.Bool.fromBool True)
        result `shouldBe` Right (PostgresqlTypes.Bool.fromBool True)
      it "param float8 roundtrip" $ \conn -> do
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::float8"
            (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            (PostgresqlTypes.Float8.fromDouble 3.14)
        result `shouldBe` Right (PostgresqlTypes.Float8.fromDouble 3.14)
      -- Multiple params / multiple columns
      it "multiple params and columns" $ \conn -> do
        let ps =
              contramap fst (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
                <> contramap snd (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            cols =
              (,)
                <$> PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
                <*> PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::int4, $2::int4"
            ps
            (PostgresqlCodecs.oneRow cols)
            (PostgresqlTypes.Int4.fromInt32 10, PostgresqlTypes.Int4.fromInt32 20)
        result `shouldBe` Right (PostgresqlTypes.Int4.fromInt32 10, PostgresqlTypes.Int4.fromInt32 20)
      -- Nullable columns
      it "nullable column non-null" $ \conn -> do
        result <-
          Adapter.execQuery conn "SELECT 42::int4" mempty (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)) ()
        result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 42))
      it "nullable column null" $ \conn -> do
        result <-
          Adapter.execQuery conn "SELECT NULL::int4" mempty (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)) ()
        result `shouldBe` Right (Nothing :: Maybe PostgresqlTypes.Int4)
      -- Nullable params
      it "nullable param Nothing" $ \conn -> do
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::int4"
            (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
            (Nothing :: Maybe PostgresqlTypes.Int4)
        result `shouldBe` Right (Nothing :: Maybe PostgresqlTypes.Int4)
      it "nullable param Just" $ \conn -> do
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::int4"
            (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
            (Just (PostgresqlTypes.Int4.fromInt32 42))
        result `shouldBe` Right (Just (PostgresqlTypes.Int4.fromInt32 42))
      -- Arrays
      it "array d1 nonNullable roundtrip" $ \conn -> do
        let input = Vector.fromList [PostgresqlTypes.Int4.fromInt32 1, PostgresqlTypes.Int4.fromInt32 2, PostgresqlTypes.Int4.fromInt32 3]
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::int4[]"
            (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
            input
        result `shouldBe` Right input
      it "array d1 nullable elements" $ \conn -> do
        let input = Vector.fromList [Just (PostgresqlTypes.Int4.fromInt32 1), Nothing, Just (PostgresqlTypes.Int4.fromInt32 3)]
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::int4[]"
            (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
            input
        result `shouldBe` Right input
      -- Enum type
      it "enum roundtrip" $ \conn -> do
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::genre"
            (PostgresqlCodecs.param genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            Comedy
        result `shouldBe` Right Comedy
      -- Composite type
      it "composite roundtrip" $ \conn -> do
        let pt = CompositePoint {ptX = PostgresqlTypes.Int4.fromInt32 42, ptY = PostgresqlTypes.Int4.fromInt32 7}
        result <-
          Adapter.execQuery
            conn
            "SELECT $1::composite_point"
            (PostgresqlCodecs.param compositePointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
            (PostgresqlCodecs.oneRow (PostgresqlCodecs.column compositePointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
            pt
        result `shouldBe` Right pt
