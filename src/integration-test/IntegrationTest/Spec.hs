module IntegrationTest.Spec (spec) where

import Prelude hiding (Int8)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32, Int64)
import Data.Profunctor (lmap)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import PostgresqlCodecs
import Test.Tasty
import Test.Tasty.HUnit
import TestContainers.Monad (TestContainer)
import TestcontainersPostgresql (Config (..), Auth (..), setup)
import TestContainers.Tasty (withContainers)
import IntegrationTest.Adapter (execQuery)

-- ---------------------------------------------------------------------------
-- Custom scalar types for testing

data Genre = Comedy | Action | Drama
  deriving (Eq, Show)

genreScalar :: Scalar Genre
genreScalar =
  enum
    "public"
    "genre"
    [("Comedy", Comedy), ("Action", Action), ("Drama", Drama)]

data CompositePoint = CompositePoint {ptX :: Int32, ptY :: Int32}
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
          result @?= Right (42 :: Int32),
        testCase "someRow with result" $ do
          conn <- getConn
          result <-
            execQuery conn "SELECT 42::int4" mempty (someRow (column native d0 nonNullable)) ()
          result @?= Right (Just (42 :: Int32)),
        testCase "someRow without result" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT 42::int4 WHERE false"
              mempty
              (someRow (column native d0 nonNullable))
              ()
          result @?= Right (Nothing :: Maybe Int32),
        testCase "manyRows" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT * FROM unnest(ARRAY[1,2,3]::int4[])"
              mempty
              (manyRows (column native d0 nonNullable))
              ()
          result @?= Right (Vector.fromList [1, 2, 3] :: Vector.Vector Int32),
        -- Scalar parameter roundtrips
        testCase "param int4 roundtrip" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::int4"
              (param native d0 nonNullable)
              (oneRow (column native d0 nonNullable))
              (42 :: Int32)
          result @?= Right (42 :: Int32),
        testCase "param int8 roundtrip" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::int8"
              (param native d0 nonNullable)
              (oneRow (column native d0 nonNullable))
              (9876543210 :: Int64)
          result @?= Right (9876543210 :: Int64),
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
              True
          result @?= Right True,
        testCase "param float8 roundtrip" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::float8"
              (param native d0 nonNullable)
              (oneRow (column native d0 nonNullable))
              (3.14 :: Double)
          result @?= Right (3.14 :: Double),
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
              (10 :: Int32, 20 :: Int32)
          result @?= Right (10 :: Int32, 20 :: Int32),
        -- Nullable columns
        testCase "nullable column non-null" $ do
          conn <- getConn
          result <-
            execQuery conn "SELECT 42::int4" mempty (oneRow (column native d0 nullable)) ()
          result @?= Right (Just (42 :: Int32)),
        testCase "nullable column null" $ do
          conn <- getConn
          result <-
            execQuery conn "SELECT NULL::int4" mempty (oneRow (column native d0 nullable)) ()
          result @?= Right (Nothing :: Maybe Int32),
        -- Nullable params
        testCase "nullable param Nothing" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::int4"
              (param native d0 nullable)
              (oneRow (column native d0 nullable))
              (Nothing :: Maybe Int32)
          result @?= Right (Nothing :: Maybe Int32),
        testCase "nullable param Just" $ do
          conn <- getConn
          result <-
            execQuery
              conn
              "SELECT $1::int4"
              (param native d0 nullable)
              (oneRow (column native d0 nullable))
              (Just (42 :: Int32))
          result @?= Right (Just (42 :: Int32)),
        -- Arrays
        testCase "array d1 nonNullable roundtrip" $ do
          conn <- getConn
          let input = Vector.fromList [1, 2, 3] :: Vector.Vector Int32
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
          let input = Vector.fromList [Just 1, Nothing, Just 3] :: Vector.Vector (Maybe Int32)
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
          let pt = CompositePoint {ptX = 42, ptY = 7}
          result <-
            execQuery
              conn
              "SELECT $1::composite_point"
              (param compositePointScalar d0 nonNullable)
              (oneRow (column compositePointScalar d0 nonNullable))
              pt
          result @?= Right pt
      ]
