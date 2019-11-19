{-|
Description : Fixtures for bringing up PostgreSQL clusters.

-}
module Database.Postgres.Fixture
  ( ephemeralCluster,
    simpleConnection,
    setupProxyDatabase,
    augmentEnvironment,
    Schema
    )
where

import qualified Data.Acquire
import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Simple.Types as Simple.Types
import qualified Dict
import qualified Log (unSecret)
import qualified MySQL.Settings
import qualified Nri.FileSystem as FileSystem
import Nri.Prelude
import qualified Postgres.Settings as Settings
import qualified System.Environment as Environment
import System.FilePath ((</>))
import qualified System.Posix.User as User
import qualified System.Process.Typed as Process
import Text.Printf (printf)

type Schema = Text

ephemeralCluster :: Data.Acquire.Acquire Settings.ConnectionSettings
ephemeralCluster = ephemeralCluster' []

ephemeralCluster' :: [([Char], [Char])] -> Data.Acquire.Acquire Settings.ConnectionSettings
ephemeralCluster' extraEnv = do
  dataDir <- FileSystem.temporaryDirectory
  Data.Acquire.mkAcquire (acquire dataDir) (release dataDir)
  where
    acquire :: FilePath -> IO Settings.ConnectionSettings
    acquire dataDir = do
      env <- augmentEnvironment extraEnv
      Process.runProcess_
        <| Process.setEnv env
        <| Process.proc "pg_ctl"
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
        <| Process.setEnv env
        <| Process.proc "pg_ctl"
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
      effectiveUserName <- map toS User.getEffectiveUserName
      pure
        (Settings.defaultSettings |> Settings.pgConnection)
          { Settings.pgHost = Settings.PgHost (toS dataDir),
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
        <| Process.setEnv env
        <| Process.proc "pg_ctl" ["stop", "--pgdata", dataDir]
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
          { Simple.connectHost = toS unPgHost,
            Simple.connectPort = fromIntegral unPgPort,
            Simple.connectUser = toS unPgUser,
            Simple.connectPassword = toS <| Log.unSecret unPgPassword,
            Simple.connectDatabase = toS unPgDatabase
            }
      release :: Simple.Connection -> IO ()
      release =
        Simple.close

setupProxyDatabase
  :: [Schema]
  -> Settings.ConnectionSettings
  -> MySQL.Settings.Settings
  -> Data.Acquire.Acquire Settings.ConnectionSettings
setupProxyDatabase
  schemas
  Settings.ConnectionSettings
    { Settings.pgHost,
      Settings.pgPort,
      Settings.pgDatabase,
      Settings.pgUser,
      Settings.pgPassword
      }
  MySQL.Settings.Settings
    { MySQL.Settings.database = database,
      MySQL.Settings.user,
      MySQL.Settings.password,
      MySQL.Settings.connection = mysqlConnection
      } = do
    ephemeralConnectionSettings <-
      case mysqlConnection of
        MySQL.Settings.ConnectTcp _ _ ->
          ephemeralCluster' []
        MySQL.Settings.ConnectSocket socket ->
          -- The `mysql_fdw` extension doesn't give the option of connecting to
          -- a UNIX socket. However, it uses the MySQL C client library which
          -- does recognize the `MYSQL_UNIX_PORT` environment variable, so we
          -- can get around that limitation.
          ephemeralCluster' [("MYSQL_UNIX_PORT", MySQL.Settings.unSocket socket)]
    Data.Acquire.mkAcquire (acquire ephemeralConnectionSettings) release
    where
      acquire :: Settings.ConnectionSettings -> IO Settings.ConnectionSettings
      acquire connectionSettings = do
        Data.Acquire.with (simpleConnection connectionSettings) configure
        pure connectionSettings
      configure :: Simple.Connection -> IO ()
      configure ephemeralConnection = do
        --
        -- Configure the connection to the "monolith" MySQL.
        --
        _ <-
          Simple.execute_ ephemeralConnection
            "CREATE EXTENSION IF NOT EXISTS mysql_fdw"
        _ <-
          case mysqlConnection of
            MySQL.Settings.ConnectTcp host port ->
              Simple.execute ephemeralConnection
                "CREATE SERVER monolith FOREIGN DATA WRAPPER mysql_fdw OPTIONS (host ?, port ?)"
                [MySQL.Settings.unHost host, show (MySQL.Settings.unPort port)]
            MySQL.Settings.ConnectSocket _socket ->
              Simple.execute_ ephemeralConnection
                "CREATE SERVER monolith FOREIGN DATA WRAPPER mysql_fdw OPTIONS (host 'localhost', port '0')"
        _ <-
          Simple.execute ephemeralConnection
            "CREATE USER MAPPING FOR CURRENT_USER SERVER monolith OPTIONS (username ?, password ?)"
            ( MySQL.Settings.unUser user,
              Log.unSecret (MySQL.Settings.unPassword password)
              )
        _ <-
          Simple.execute_ ephemeralConnection
            "CREATE SCHEMA monolith"
        _ <-
          Simple.execute ephemeralConnection
            "IMPORT FOREIGN SCHEMA ? FROM SERVER monolith INTO monolith"
            [Simple.Types.Identifier (MySQL.Settings.unDatabase database)]
        --
        -- Configure the connection to the "app" PostgreSQL instance.
        --
        _ <-
          Simple.execute_ ephemeralConnection
            "CREATE EXTENSION IF NOT EXISTS postgres_fdw"
        _ <-
          Simple.execute ephemeralConnection
            "CREATE SERVER app FOREIGN DATA WRAPPER postgres_fdw OPTIONS (host ?, dbname ?, port ?)"
            [ Settings.unPgHost pgHost,
              Settings.unPgDatabase pgDatabase,
              show (Settings.unPgPort pgPort)
              ]
        _ <-
          Simple.execute ephemeralConnection
            "CREATE USER MAPPING FOR CURRENT_USER SERVER app OPTIONS (user ?, password ?)"
            ( Settings.unPgUser pgUser,
              Log.unSecret (Settings.unPgPassword pgPassword)
              )
        for_ schemas <| \schema -> do
          _ <-
            Simple.execute ephemeralConnection
              "CREATE SCHEMA ?"
              [Simple.Types.Identifier schema]
          _ <-
            Simple.execute ephemeralConnection
              "IMPORT FOREIGN SCHEMA ? FROM SERVER app INTO ?"
              [Simple.Types.Identifier schema, Simple.Types.Identifier schema]
          pure ()
        pure ()
      release :: Settings.ConnectionSettings -> IO ()
      release _ =
        pure ()

augmentEnvironment :: [([Char], [Char])] -> IO [([Char], [Char])]
augmentEnvironment overrides = do
  environment <- Environment.getEnvironment
  let overridesDict = Dict.fromList overrides
  let environmentDict = Dict.fromList environment
  let combinedDict = Dict.union overridesDict environmentDict
  pure <| Dict.toList combinedDict
