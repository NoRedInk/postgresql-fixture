module Database.PostgreSQL.Fixture
  ( ephemeralCluster,
    simpleConnection,
  )
where

import qualified Data.Acquire
import Database.PostgreSQL.Fixture.Ephemeral (ephemeralCluster)
import qualified Database.PostgreSQL.Fixture.Settings as Settings
import qualified Database.PostgreSQL.Simple as Simple

-- Ideas for top-level API:
-- cluster :: Cluster -> Data.Acquire.Acquire Settings.ConnectionSettings
-- ephemeralCluster :: Data.Acquire.Acquire Settings.ConnectionSettings
-- acquireCluster :: Cluster -> Consumer -> IO Settings.ConnectionSettings
-- releaseCluster :: Cluster -> Consumer -> IO ()

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
