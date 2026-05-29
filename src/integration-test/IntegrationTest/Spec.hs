module IntegrationTest.Spec (spec) where

import Prelude hiding (Bool, Int8)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant (contramap)
import Data.Profunctor (lmap)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import PostgresqlCodecs
import PostgresqlTypes.Bool (Bool (Bool))
import PostgresqlTypes.Float8 (Float8 (Float8))
import PostgresqlTypes.Int4 (Int4 (Int4))
import PostgresqlTypes.Int8 (Int8 (Int8))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (NumThreads (..))
import TestContainers.Monad (TestContainer)
import TestcontainersPostgresql (Config (..), Auth (..), setup)
import TestContainers.Tasty (withContainers)
import IntegrationTest.Adapter (execQuery)

-- ---------------------------------------------------------------------------
-- Custom scalar types for testing

data Genre = Comedy | Action | Drama
  deriving (Eq, Ord, Show)

genreScalar :: Scalar Genre
genreScalar =
  enum
    "public"
    "genre"
    [("Comedy", Comedy), ("Action", Action), ("Drama", Drama)]

data CompositePoint = CompositePoint {ptX :: Int4, ptY :: Int4}
  deriving (Eq, Show)

compositePointScalar :: Scalar CompositePoint
compositePointScalar =
  composite
    "public"
    "composite_point"
    ( CompositePoint
        <$> lmap (.ptX) (field native d0 nonNullable)
        <*> lmap (.ptY) (field native d0 nonNullable)
    )

-- ---------------------------------------------------------------------------
-- Container setup

pgConfig :: Config
pgConfig = Config {tagName = "postgres:16", auth = TrustAuth, forwardLogs = False}

setupContainer :: TestContainer PQ.Connection
setupContainer = do
  (host, port) <- setup pgConfig
  let connStr =
        "host=" <> BS8.pack (Text.unpack host)
          <> " port=" <> BS8.pack (show port)
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
  withContainers setupContainer $ \getConn ->
    testGroup
      "Integration"
      [ -- Result kinds
        testCase "rowsAffected" $ do
          conn <- getConn
          _ <- PQ.exec conn "CREATE TEMP TABLE t_affected (x int4)"
          result <-
            execQuery
              conn
              "INSERT INTO t_affected SELECT generate_series(1, 5)"
              mempty
              rowsAffected
              ()
          result @?= Right 5,
        testCase "oneRow int4" $ do
          conn <- getConn
          result <-
            execQuery conn "SELECT 42::int4" mempty (oneRow (column native d0 nonNullable)) ()
          result @?= Right (Int4 42),
        testCase "someRow with result" $ do
          conn <- getConn
          result <-
            execQuery conn "SELECT 42::int4" mempty (someRow (column native d0 nonNullable)) ()
          result @?= Right (Just (Int4 42)),
        testCase "someRow without result" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT 42::int4 WHERE false"
              mempty
              (someRow (column native d0 nonNullable))
              ()
          result @?= Right (Nothing :: Maybe Int4),
        testCase "manyRows" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT * FROM unnest(ARRAY[1,2,3]::int4[])"
              mempty
              (manyRows (column native d0 nonNullable))
              ()
          result @?= Right (Vector.fromList [Int4 1, Int4 2, Int4 3]),
        -- Scalar parameter roundtrips
        testCase "param int4 roundtrip" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::int4"
              (param native d0 nonNullable)
              (oneRow (column native d0 nonNullable))
              (Int4 42)
          result @?= Right (Int4 42),
        testCase "param int8 roundtrip" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::int8"
              (param native d0 nonNullable)
              (oneRow (column native d0 nonNullable))
              (Int8 9876543210)
          result @?= Right (Int8 9876543210),
        testCase "param text roundtrip" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::text"
              (param native d0 nonNullable)
              (oneRow (column native d0 nonNullable))
              ("hello" :: Text)
          result @?= Right ("hello" :: Text),
        testCase "param bool roundtrip" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::bool"
              (param native d0 nonNullable)
              (oneRow (column native d0 nonNullable))
              (Bool True)
          result @?= Right (Bool True),
        testCase "param float8 roundtrip" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::float8"
              (param native d0 nonNullable)
              (oneRow (column native d0 nonNullable))
              (Float8 3.14)
          result @?= Right (Float8 3.14),
        -- Multiple params / multiple columns
        testCase "multiple params and columns" $ do
          conn <- getConn
          let ps =
                contramap fst (param native d0 nonNullable)
                  <> contramap snd (param native d0 nonNullable)
              cols =
                (,)
                  <$> column native d0 nonNullable
                  <*> column native d0 nonNullable
          result <-
            execQuery
              conn
              "SELECT $1::int4, $2::int4"
              ps
              (oneRow cols)
              (Int4 10, Int4 20)
          result @?= Right (Int4 10, Int4 20),
        -- Nullable columns
        testCase "nullable column non-null" $ do
          conn <- getConn
          result <-
            execQuery conn "SELECT 42::int4" mempty (oneRow (column native d0 nullable)) ()
          result @?= Right (Just (Int4 42)),
        testCase "nullable column null" $ do
          conn <- getConn
          result <-
            execQuery conn "SELECT NULL::int4" mempty (oneRow (column native d0 nullable)) ()
          result @?= Right (Nothing :: Maybe Int4),
        -- Nullable params
        testCase "nullable param Nothing" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::int4"
              (param native d0 nullable)
              (oneRow (column native d0 nullable))
              (Nothing :: Maybe Int4)
          result @?= Right (Nothing :: Maybe Int4),
        testCase "nullable param Just" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::int4"
              (param native d0 nullable)
              (oneRow (column native d0 nullable))
              (Just (Int4 42))
          result @?= Right (Just (Int4 42)),
        -- Arrays
        testCase "array d1 nonNullable roundtrip" $ do
          conn <- getConn
          let input = Vector.fromList [Int4 1, Int4 2, Int4 3]
          result <-
            execQuery
              conn
              "SELECT $1::int4[]"
              (param native (d1 nonNullable vector) nonNullable)
              (oneRow (column native (d1 nonNullable vector) nonNullable))
              input
          result @?= Right input,
        testCase "array d1 nullable elements" $ do
          conn <- getConn
          let input = Vector.fromList [Just (Int4 1), Nothing, Just (Int4 3)]
          result <-
            execQuery
              conn
              "SELECT $1::int4[]"
              (param native (d1 nullable vector) nonNullable)
              (oneRow (column native (d1 nullable vector) nonNullable))
              input
          result @?= Right input,
        -- Enum type
        testCase "enum roundtrip" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::genre"
              (param genreScalar d0 nonNullable)
              (oneRow (column genreScalar d0 nonNullable))
              Comedy
          result @?= Right Comedy,
        -- Composite type
        testCase "composite roundtrip" $ do
          conn <- getConn
          let pt = CompositePoint {ptX = Int4 42, ptY = Int4 7}
          result <-
            execQuery
              conn
              "SELECT $1::composite_point"
              (param compositePointScalar d0 nonNullable)
              (oneRow (column compositePointScalar d0 nonNullable))
              pt
          result @?= Right pt
      ]
