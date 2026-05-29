{-# LANGUAGE OverloadedRecordDot #-}

module IntegrationTest.Prelude
  ( Genre (..),
    genreScalar,
    CompositePoint (..),
    compositePointScalar,
    CompositeContainer (..),
    compositeContainerScalar,
    pgConfig,
    setupContainer,
    query,
    queryParams,
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Profunctor
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified IntegrationTest.Adapter as Adapter
import qualified PostgresqlCodecs
import qualified PostgresqlTypes
import Test.Hspec
import qualified TestContainers.Hspec as TestContainers
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
    _ <- PQ.exec conn "CREATE TYPE composite_container AS (point composite_point, label text)"
    pure ()
  pure conn

-- ---------------------------------------------------------------------------
-- Query helpers

query :: PQ.Connection -> BS.ByteString -> PostgresqlCodecs.Result result -> IO (Either Text result)
query conn sql result = Adapter.execQuery conn sql mempty result ()

queryParams :: PQ.Connection -> BS.ByteString -> PostgresqlCodecs.Params params -> PostgresqlCodecs.Result result -> params -> IO (Either Text result)
queryParams = Adapter.execQuery
