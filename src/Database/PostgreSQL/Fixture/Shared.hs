module Database.PostgreSQL.Fixture.Shared
  ( withCluster,
    useCluster,
    releaseCluster,
  )
where

import qualified Data.Acquire
import Data.Text (Text, unpack)
import qualified Database.PostgreSQL.Fixture.Cluster as Cluster
import Database.PostgreSQL.Fixture.Cluster (Cluster (Cluster))
import qualified Database.PostgreSQL.Fixture.Consumers as Consumers
import Database.PostgreSQL.Fixture.Settings (ConnectionSettings)
import System.FilePath ((</>), FilePath)

withCluster :: FilePath -> Data.Acquire.Acquire ConnectionSettings
withCluster dataDir =
  Data.Acquire.mkAcquire acquire release
  where
    cluster =
      Cluster dataDir []
    resource =
      Consumers.Resource $ dataDir </> "consumers"
    consumer =
      Consumers.Runtime
    acquire :: IO ConnectionSettings
    acquire =
      Cluster.lockExclusive cluster $ do
        Cluster.create cluster
        Consumers.acquire consumer resource
        Cluster.start cluster
    release :: ConnectionSettings -> IO ()
    release _ =
      Cluster.lockExclusive cluster $ do
        shouldStop <- not <$> Consumers.inUse resource
        if shouldStop
          then Cluster.stop cluster
          else pure ()
        Consumers.release consumer resource

useCluster :: FilePath -> Text -> IO ConnectionSettings
useCluster dataDir consumerName =
  Cluster.lockExclusive cluster $ do
    Cluster.create cluster
    Consumers.acquire consumer resource
    Cluster.start cluster
  where
    cluster =
      Cluster dataDir []
    resource =
      Consumers.Resource $ dataDir </> "consumers"
    consumer =
      Consumers.Persistent $ unpack consumerName

releaseCluster :: FilePath -> Text -> IO ()
releaseCluster dataDir consumerName =
  Cluster.lockExclusive cluster $ do
    shouldStop <- not <$> Consumers.inUse resource
    if shouldStop
      then Cluster.stop cluster
      else pure ()
    Consumers.release consumer resource
  where
    cluster =
      Cluster dataDir []
    resource =
      Consumers.Resource $ dataDir </> "consumers"
    consumer =
      Consumers.Persistent $ unpack consumerName
