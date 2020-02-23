-- |
-- Description : Fixtures for bringing up PostgreSQL clusters.
module Database.PostgreSQL.Fixture
  ( ephemeralCluster,
    simpleConnection,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Acquire
import qualified Database.PostgreSQL.Cluster as Cluster
import Database.PostgreSQL.Cluster (Cluster (Cluster))
import qualified Database.PostgreSQL.Fixture.Settings as Settings
import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Simple.Types as Simple.Types
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import qualified System.Environment as Environment
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)
import qualified System.Process.Typed as Process
import Text.Printf (printf)

ephemeralCluster :: Data.Acquire.Acquire Settings.ConnectionSettings
ephemeralCluster = ephemeralCluster' []

ephemeralCluster' :: [(String, String)] -> Data.Acquire.Acquire Settings.ConnectionSettings
ephemeralCluster' extraEnv = do
  tempDir <- temporaryDirectory
  let dataDir = tempDir </> "cluster"
  let cluster = Cluster dataDir extraEnv
  Data.Acquire.mkAcquire (acquire cluster) (release cluster)
  where
    acquire :: Cluster -> IO Settings.ConnectionSettings
    acquire cluster = do
      Cluster.create cluster
      Cluster.start cluster
    release :: Cluster -> Settings.ConnectionSettings -> IO ()
    release cluster _ = do
      Cluster.stop cluster
      Cluster.destroy cluster

simpleConnection :: Settings.ConnectionSettings -> Data.Acquire.Acquire Simple.Connection
simpleConnection
  Settings.ConnectionSettings
    { Settings.pgHost = Settings.PgHost {Settings.unPgHost},
      Settings.pgPort = Settings.PgPort {Settings.unPgPort},
      Settings.pgUser = Settings.PgUser {Settings.unPgUser},
      Settings.pgPassword = Settings.PgPassword {Settings.unPgPassword},
      Settings.pgDatabase = Settings.PgDatabase {Settings.unPgDatabase}
    } =
    Data.Acquire.mkAcquire acquire release
    where
      acquire :: IO Simple.Connection
      acquire =
        Simple.connect Simple.ConnectInfo
          { Simple.connectHost = unPgHost,
            Simple.connectPort = fromIntegral unPgPort,
            Simple.connectUser = unPgUser,
            Simple.connectPassword = unPgPassword,
            Simple.connectDatabase = unPgDatabase
          }
      release :: Simple.Connection -> IO ()
      release =
        Simple.close

temporaryDirectory :: Data.Acquire.Acquire FilePath
temporaryDirectory = do
  -- NOTE: keep the temporary path short because UNIX domain socket paths are
  -- limited to 103 bytes on macOS (maybe it's different on Linux) and macOS
  -- likes to set $TMP to /var/folders/83/p3y5dr1s0zj0w95s7t_mzwcc0000gk/T/ or
  -- something bizarre like that.
  tmp <- liftIO getTemporaryDirectory
  Data.Acquire.mkAcquire
    (mkdtemp (tmp </> "pg."))
    removeDirectoryRecursive
