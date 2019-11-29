-- |
-- Description : Fixtures for bringing up PostgreSQL clusters.
module Database.Postgres.Fixture
  ( ephemeralCluster,
    simpleConnection,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Acquire
import qualified Data.Map.Strict as Dict
import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Simple.Types as Simple.Types
import qualified Database.Postgres.Fixture.Settings as Settings
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import qualified System.Environment as Environment
import System.FilePath ((</>))
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)
import qualified System.Posix.User as User
import qualified System.Process.Typed as Process
import Text.Printf (printf)

ephemeralCluster :: Data.Acquire.Acquire Settings.ConnectionSettings
ephemeralCluster = ephemeralCluster' []

ephemeralCluster' :: [(String, String)] -> Data.Acquire.Acquire Settings.ConnectionSettings
ephemeralCluster' extraEnv = do
  dataDir <- temporaryDirectory
  Data.Acquire.mkAcquire (acquire dataDir) (release dataDir)
  where
    acquire :: FilePath -> IO Settings.ConnectionSettings
    acquire dataDir = do
      env <- augmentEnvironment extraEnv
      Process.runProcess_
        $ Process.setEnv env
        $ Process.proc
          "pg_ctl"
          [ "init",
            "--pgdata",
            dataDir,
            --
            -- Explanation of the options being passed in via `-o`:
            --
            -- --auth trust    -> Trust all local users, i.e. authentication is
            --                    based on UNIX filesystem permissions.
            --
            -- --encoding utf8 -> Ensure that the template is using UTF-8, and
            --                    hence subsequent databases (unless overridden).
            --
            -- See initdb(1) for more information on these flags.
            --
            "-o",
            "--auth trust --encoding utf8"
          ]
      Process.runProcess_
        $ Process.setEnv env
        $ Process.proc
          "pg_ctl"
          [ "start",
            "-w",
            "--pgdata",
            dataDir,
            "-l",
            dataDir </> "log",
            --
            -- Explanation of the options being passed in via `-o`:
            --
            -- -h ''   -> Disable all TCP connections.
            --
            -- -p NNNN -> Listen on port NNNN. Because we're only using UNIX
            --            sockets, this is actually the suffix used in the socket
            --            file name, i.e. in `.s.PGSQL.NNNN`.
            --
            -- -k dir  -> The directory in which to put the UNIX socket. Note that
            --            we don't shell-escape the `dataDir` directory we pass to
            --            the `-k` argument out of laziness. We don't expect these
            --            temporary directories to have spaces or weird characters
            --            in them, so we should be good.
            --
            -- See postgres(1) for more information on these flags.
            --
            "-o",
            printf "-h '' -p %u -k %s" arbitraryPort dataDir
          ]
      -- Should this be `getLoginName` or `getEffectiveUserName`? Practically,
      -- for our purposes, it should not matter – they should be the same – so
      -- is this just a question of correctness? No: `getLoginName` only works
      -- when stdin is connected to terminal, so `getEffectiveUserName` is what
      -- we use here, otherwise this will not work in CI or when deployed.
      effectiveUserName <- User.getEffectiveUserName
      pure
        (Settings.pgConnection Settings.defaultSettings)
          { Settings.pgHost = Settings.PgHost dataDir,
            Settings.pgPort = Settings.PgPort arbitraryPort,
            Settings.pgUser = Settings.PgUser effectiveUserName,
            -- In a new cluster there are `template0` and `template1` databases.
            -- The latter is the "right" place to start.
            Settings.pgDatabase = Settings.PgDatabase "template1"
          }
    release :: FilePath -> Settings.ConnectionSettings -> IO ()
    release dataDir _ = do
      env <- augmentEnvironment extraEnv
      Process.runProcess_
        $ Process.setEnv env
        $ Process.proc "pg_ctl" ["stop", "--pgdata", dataDir]
    arbitraryPort :: Int
    arbitraryPort = 5432

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

augmentEnvironment :: [(String, String)] -> IO [(String, String)]
augmentEnvironment overrides = do
  environment <- Environment.getEnvironment
  let overridesDict = Dict.fromList overrides
  let environmentDict = Dict.fromList environment
  let combinedDict = Dict.union overridesDict environmentDict
  pure $ Dict.toList combinedDict

temporaryDirectory :: Data.Acquire.Acquire FilePath
temporaryDirectory = do
  tmp <- liftIO getTemporaryDirectory
  Data.Acquire.mkAcquire (mkdtemp (tmp </> "database-postgres-fixture-")) removeDirectoryRecursive
