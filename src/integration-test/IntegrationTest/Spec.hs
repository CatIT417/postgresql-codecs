module IntegrationTest.Spec (spec) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Contravariant (contramap)
import Data.Profunctor (lmap)
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
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (NumThreads (..))
import qualified TestContainers.Monad as TestContainers
import qualified TestContainers.Tasty as TestContainers
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
-- Test tree

spec :: TestTree
spec =
  localOption (NumThreads 1) $
    TestContainers.withContainers setupContainer $ \getConn ->
      testGroup
        "Integration"
        [ -- Result kinds
          testCase "rowsAffected" $ do
            conn <- getConn
            _ <- PQ.exec conn "CREATE TEMP TABLE t_affected (x int4)"
            result <-
              Adapter.execQuery
                conn
                "INSERT INTO t_affected SELECT generate_series(1, 5)"
                mempty
                PostgresqlCodecs.rowsAffected
                ()
            result @?= Right 5,
          testCase "oneRow int4" $ do
            conn <- getConn
            result <-
              Adapter.execQuery conn "SELECT 42::int4" mempty (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)) ()
            result @?= Right (PostgresqlTypes.Int4.fromInt32 42),
          testCase "someRow with result" $ do
            conn <- getConn
            result <-
              Adapter.execQuery conn "SELECT 42::int4" mempty (PostgresqlCodecs.someRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)) ()
            result @?= Right (Just (PostgresqlTypes.Int4.fromInt32 42)),
          testCase "someRow without result" $ do
            conn <- getConn
            result <-
              Adapter.execQuery
                conn
                "SELECT 42::int4 WHERE false"
                mempty
                (PostgresqlCodecs.someRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
                ()
            result @?= Right (Nothing :: Maybe PostgresqlTypes.Int4),
          testCase "manyRows" $ do
            conn <- getConn
            result <-
              Adapter.execQuery
                conn
                "SELECT * FROM unnest(ARRAY[1,2,3]::int4[])"
                mempty
                (PostgresqlCodecs.manyRows (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
                ()
            result @?= Right (Vector.fromList [PostgresqlTypes.Int4.fromInt32 1, PostgresqlTypes.Int4.fromInt32 2, PostgresqlTypes.Int4.fromInt32 3]),
          -- Scalar parameter roundtrips
          testCase "param int4 roundtrip" $ do
            conn <- getConn
            result <-
              Adapter.execQuery
                conn
                "SELECT $1::int4"
                (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
                (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
                (PostgresqlTypes.Int4.fromInt32 42)
            result @?= Right (PostgresqlTypes.Int4.fromInt32 42),
          testCase "param int8 roundtrip" $ do
            conn <- getConn
            result <-
              Adapter.execQuery
                conn
                "SELECT $1::int8"
                (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
                (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
                (PostgresqlTypes.Int8.fromInt64 9876543210)
            result @?= Right (PostgresqlTypes.Int8.fromInt64 9876543210),
          testCase "param text roundtrip" $ do
            conn <- getConn
            result <-
              Adapter.execQuery
                conn
                "SELECT $1::text"
                (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
                (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
                ("hello" :: PostgresqlTypes.Text)
            result @?= Right ("hello" :: PostgresqlTypes.Text),
          testCase "param bool roundtrip" $ do
            conn <- getConn
            result <-
              Adapter.execQuery
                conn
                "SELECT $1::bool"
                (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
                (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
                (PostgresqlTypes.Bool.fromBool True)
            result @?= Right (PostgresqlTypes.Bool.fromBool True),
          testCase "param float8 roundtrip" $ do
            conn <- getConn
            result <-
              Adapter.execQuery
                conn
                "SELECT $1::float8"
                (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
                (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
                (PostgresqlTypes.Float8.fromDouble 3.14)
            result @?= Right (PostgresqlTypes.Float8.fromDouble 3.14),
          -- Multiple params / multiple columns
          testCase "multiple params and columns" $ do
            conn <- getConn
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
            result @?= Right (PostgresqlTypes.Int4.fromInt32 10, PostgresqlTypes.Int4.fromInt32 20),
          -- Nullable columns
          testCase "nullable column non-null" $ do
            conn <- getConn
            result <-
              Adapter.execQuery conn "SELECT 42::int4" mempty (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)) ()
            result @?= Right (Just (PostgresqlTypes.Int4.fromInt32 42)),
          testCase "nullable column null" $ do
            conn <- getConn
            result <-
              Adapter.execQuery conn "SELECT NULL::int4" mempty (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)) ()
            result @?= Right (Nothing :: Maybe PostgresqlTypes.Int4),
          -- Nullable params
          testCase "nullable param Nothing" $ do
            conn <- getConn
            result <-
              Adapter.execQuery
                conn
                "SELECT $1::int4"
                (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
                (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
                (Nothing :: Maybe PostgresqlTypes.Int4)
            result @?= Right (Nothing :: Maybe PostgresqlTypes.Int4),
          testCase "nullable param Just" $ do
            conn <- getConn
            result <-
              Adapter.execQuery
                conn
                "SELECT $1::int4"
                (PostgresqlCodecs.param PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable)
                (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native PostgresqlCodecs.d0 PostgresqlCodecs.nullable))
                (Just (PostgresqlTypes.Int4.fromInt32 42))
            result @?= Right (Just (PostgresqlTypes.Int4.fromInt32 42)),
          -- Arrays
          testCase "array d1 nonNullable roundtrip" $ do
            conn <- getConn
            let input = Vector.fromList [PostgresqlTypes.Int4.fromInt32 1, PostgresqlTypes.Int4.fromInt32 2, PostgresqlTypes.Int4.fromInt32 3]
            result <-
              Adapter.execQuery
                conn
                "SELECT $1::int4[]"
                (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
                (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nonNullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
                input
            result @?= Right input,
          testCase "array d1 nullable elements" $ do
            conn <- getConn
            let input = Vector.fromList [Just (PostgresqlTypes.Int4.fromInt32 1), Nothing, Just (PostgresqlTypes.Int4.fromInt32 3)]
            result <-
              Adapter.execQuery
                conn
                "SELECT $1::int4[]"
                (PostgresqlCodecs.param PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable)
                (PostgresqlCodecs.oneRow (PostgresqlCodecs.column PostgresqlCodecs.native (PostgresqlCodecs.d1 PostgresqlCodecs.nullable PostgresqlCodecs.vector) PostgresqlCodecs.nonNullable))
                input
            result @?= Right input,
          -- Enum type
          testCase "enum roundtrip" $ do
            conn <- getConn
            result <-
              Adapter.execQuery
                conn
                "SELECT $1::genre"
                (PostgresqlCodecs.param genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
                (PostgresqlCodecs.oneRow (PostgresqlCodecs.column genreScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
                Comedy
            result @?= Right Comedy,
          -- Composite type
          testCase "composite roundtrip" $ do
            conn <- getConn
            let pt = CompositePoint {ptX = PostgresqlTypes.Int4.fromInt32 42, ptY = PostgresqlTypes.Int4.fromInt32 7}
            result <-
              Adapter.execQuery
                conn
                "SELECT $1::composite_point"
                (PostgresqlCodecs.param compositePointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable)
                (PostgresqlCodecs.oneRow (PostgresqlCodecs.column compositePointScalar PostgresqlCodecs.d0 PostgresqlCodecs.nonNullable))
                pt
            result @?= Right pt
        ]
