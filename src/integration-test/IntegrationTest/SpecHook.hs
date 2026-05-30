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

data Env = Env
  { pool :: Pool.Pool PQ.Connection,
    binary :: Bool
  }

type HookedSpec = SpecWith Env

setupContainer :: TestContainers.TestContainer (Pool.Pool PQ.Connection)
setupContainer = do
  (host, port) <- TcPg.setup config
  liftIO $ Pool.newPool $ Pool.defaultPoolConfig (connect host port) PQ.finish 30 100
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
hook hookedSpec =
  aroundAll (TestContainers.withContainers setupContainer) $
    parallel $ do
      describe "Binary format" $
        aroundWith (\action pool -> action (Env pool True)) hookedSpec
      describe "Text format" $
        aroundWith (\action pool -> action (Env pool False)) hookedSpec
