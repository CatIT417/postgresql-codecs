module IntegrationTest.SpecHook where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Database.PostgreSQL.LibPQ as PQ
import Test.Hspec
import qualified TestContainers.Hspec as TestContainers
import qualified TestcontainersPostgresql as TcPg
import Prelude

type HookedSpec = SpecWith PQ.Connection

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

hook :: HookedSpec -> Spec
hook = aroundAll (TestContainers.withContainers setupContainer)
