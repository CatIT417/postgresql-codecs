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
  pool <- liftIO $ Pool.newPool $ Pool.defaultPoolConfig (connect host port) PQ.finish 30 100
  liftIO $ Pool.withResource pool $ \conn -> mapM_ (runDdl conn) schema
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

-- | The complete schema shared by every spec, in dependency order.
--
-- It is created exactly once, before the pool is handed to the spec tree, so
-- the statements need no @IF NOT EXISTS@ \/ @duplicate_object@ guards and can
-- never race with each other.
schema :: [BS8.ByteString]
schema =
  [ "CREATE TYPE genre AS ENUM ('rock', 'pop', 'jazz')",
    "CREATE TYPE movie_genre AS ENUM ('Comedy', 'Action', 'Drama')",
    "CREATE TYPE param_color AS ENUM ('red', 'green')",
    "CREATE TYPE artist AS (name text, genre genre)",
    "CREATE TYPE composite_point AS (x int4, y int4)",
    "CREATE TYPE composite_with_nullable AS (x int4, y int4)",
    "CREATE TYPE composite_container AS (point composite_point, label text)",
    "CREATE TYPE composite_with_array AS (xs int4[])"
  ]

-- | Run one DDL statement, failing loudly if the server rejects it.
runDdl :: PQ.Connection -> BS8.ByteString -> IO ()
runDdl conn sql = do
  mResult <- PQ.exec conn sql
  case mResult of
    Nothing -> fail ("Schema setup produced no result for: " <> BS8.unpack sql)
    Just result -> do
      status <- PQ.resultStatus result
      case status of
        PQ.CommandOk -> pure ()
        _ -> do
          mError <- PQ.resultErrorMessage result
          fail
            ( "Schema setup failed for: "
                <> BS8.unpack sql
                <> "\n"
                <> maybe (show status) BS8.unpack mError
            )

hook :: HookedSpec -> Spec
hook hookedSpec =
  aroundAll (TestContainers.withContainers setupContainer) $
    parallel $ do
      describe "Binary format" $
        aroundWith (\action pool -> action (Env pool True)) hookedSpec
      describe "Text format" $
        aroundWith (\action pool -> action (Env pool False)) hookedSpec
