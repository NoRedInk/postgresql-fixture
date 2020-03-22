module Database.PostgreSQL.Fixture.Ephemeral
  ( ephemeralCluster,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Acquire
import qualified Database.PostgreSQL.Fixture.Cluster as Cluster
import Database.PostgreSQL.Fixture.Cluster (Cluster (Cluster))
import Database.PostgreSQL.Fixture.Settings (ConnectionSettings)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)

ephemeralCluster :: Data.Acquire.Acquire ConnectionSettings
ephemeralCluster = ephemeralCluster' []

ephemeralCluster' :: [(String, String)] -> Data.Acquire.Acquire ConnectionSettings
ephemeralCluster' extraEnv = do
  tempDir <- temporaryDirectory
  let dataDir = tempDir </> "cluster"
  let cluster = Cluster dataDir extraEnv
  Data.Acquire.mkAcquire (acquire cluster) (release cluster)
  where
    acquire :: Cluster -> IO ConnectionSettings
    acquire cluster =
      Cluster.lockExclusive cluster $ do
        Cluster.create cluster
        Cluster.start cluster
    release :: Cluster -> ConnectionSettings -> IO ()
    release cluster _ =
      Cluster.lockExclusive cluster $ do
        Cluster.stop cluster
        Cluster.destroy cluster

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
