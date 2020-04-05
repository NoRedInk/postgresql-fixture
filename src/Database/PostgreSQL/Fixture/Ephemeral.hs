module Database.PostgreSQL.Fixture.Ephemeral
  ( ephemeralCluster,
  )
where

import qualified Data.Acquire
import qualified Database.PostgreSQL.Fixture.Cluster as Cluster
import Database.PostgreSQL.Fixture.Cluster (Cluster (Cluster))
import Database.PostgreSQL.Fixture.Settings (ConnectionSettings)
import Database.PostgreSQL.Fixture.Util (temporaryDirectory)
import System.FilePath ((</>))

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
