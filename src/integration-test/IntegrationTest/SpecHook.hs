module IntegrationTest.SpecHook where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Database.PostgreSQL.LibPQ as PQ
import Test.Hspec
import qualified TestContainers.Hspec as TestContainers
import qualified TestcontainersPostgresql as TcPg
import Prelude

type HookedSpec = SpecWith (Pool.Pool PQ.Connection)

setupContainer :: TestContainers.TestContainer (Pool.Pool PQ.Connection)
setupContainer = do
  (host, port) <-
    TcPg.setup config
  pool <- liftIO $ Pool.newPool $ Pool.defaultPoolConfig (connect host port) PQ.finish 30 100
  pure pool
  where
    config =
      TcPg.Config
        { tagName = "postgres:18",
          auth = TcPg.TrustAuth,
          forwardLogs = False
        }
    connect host port = do
      let connStr =
            "host="
              <> BS8.pack (Text.unpack host)
              <> " port="
              <> BS8.pack (show port)
              <> " user=postgres dbname=postgres"
      conn <- PQ.connectdb connStr
      s <- PQ.status conn
      case s of
        PQ.ConnectionOk -> pure conn
        _ -> fail "Failed to connect to PostgreSQL"

hook :: HookedSpec -> Spec
hook =
  aroundAll (TestContainers.withContainers setupContainer)
    . parallel
