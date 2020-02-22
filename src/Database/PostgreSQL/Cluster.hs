{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Fixtures for bringing up PostgreSQL clusters.
module Database.PostgreSQL.Cluster
  ( Cluster (..),
    create,
    start,
    stop,
    destroy,
    Version (..),
    version,
    versionText,
    versionCompare,
    Status (..),
    status,
  )
where

import Data.Attoparsec.Text as Attoparsec
import qualified Data.Map.Strict as Dict
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Database.PostgreSQL.Fixture.Settings as Settings
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import qualified System.Environment as Environment
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((</>), FilePath)
import qualified System.Posix.User as User
import qualified System.Process.Typed as Process
import Text.Printf (printf)

data Cluster
  = Cluster
      { dataDir :: FilePath,
        environ :: [(String, String)]
      }
  deriving (Show)

create :: Cluster -> IO ()
create cluster = do
  -- TODO: Set TZ here too? See initdb(1).
  -- TODO: Set locale too? See initdb(1).
  -- TODO: Set --nosync for speed? See initdb(1).
  env <- clusterEnvironment cluster
  Process.runProcess_
    $ Process.setEnv env
    $ Process.proc
      "pg_ctl"
      [ "init",
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

start :: Cluster -> IO Settings.ConnectionSettings
start cluster@Cluster {dataDir} = do
  -- TODO: Use -w to wait for startup? See pg_ctl(1).
  -- TODO: Use -o -F to disable fsync for speed? See postgres(1).
  --       Or set `fsync = off` in postgresql.conf.
  env <- clusterEnvironment cluster
  Process.runProcess_
    $ Process.setEnv env
    $ Process.proc
      "pg_ctl"
      [ "start",
        "-w",
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
        --
        -- The parameters above can also be set in $PGDATA/postgresql.conf. See
        -- https://www.postgresql.org/docs/9.6/config-setting.html. Parameters
        -- can also be set using ALTER SYSTEM; this may get around quoting
        -- issues. See https://www.postgresql.org/docs/9.6/sql-altersystem.html.
        --
        -- -h can also be configured by setting `listen_addresses` parameter.
        --
        -- -p can also be set via `port` parameter or PGPORT environment
        --    variable, but defaults to 5432 (compiled in).
        --
        -- -k can also be configured by setting `unix_socket_directories`
        --    parameter. XXX: Also set `unix_socket_permissions` to 0700 to lock
        --    down access to just the invoking user.
        --
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
  where
    arbitraryPort = 5432

stop :: Cluster -> IO ()
stop cluster = do
  env <- clusterEnvironment cluster
  Process.runProcess_
    $ Process.setEnv env
    $ Process.proc "pg_ctl" ["stop"]

destroy :: Cluster -> IO ()
destroy Cluster {dataDir} =
  removeDirectoryRecursive dataDir

data Version
  = Version Int Int (Maybe Int)
  | Unknown Text

versionText :: Version -> Text
versionText (Version major minor Nothing) = Text.pack $ printf "%d.%d" major minor
versionText (Version major minor (Just patch)) = Text.pack $ printf "%d.%d.%d" major minor patch
versionText (Unknown version) = version <> " (unknown)"

versionParser :: Parser Version
versionParser = do
  major <- decimal
  minor <- string "." *> decimal
  patch <- option Nothing (fmap Just $ string "." *> decimal)
  endOfLine
  pure $ Version major minor patch

versionLineParser :: Parser Version
versionLineParser = do
  string "pg_ctl" *> skipSpace *> skipNonSpace *> skipSpace
  choice [versionParser, fmap Unknown $ takeTill isEndOfLine]
  where
    skipNonSpace = skipWhile (not . isHorizontalSpace)

version :: Cluster -> IO Version
version cluster = do
  env <- clusterEnvironment cluster
  stdoutRaw <-
    Process.readProcessStdout_
      $ Process.setEnv env
      $ Process.proc "pg_ctl" ["--version"]
  let stdout = decodeOutput stdoutRaw
  let version = parseOnly versionLineParser $ stdout
  pure $ case version of
    Left err -> Unknown stdout
    Right vn -> vn

versionCompare :: Version -> Version -> Maybe Ordering
versionCompare a b =
  case (a, b) of
    (Version majora minora patcha, Version majorb minorb patchb) ->
      case compare majora majorb of
        EQ -> case compare minora minorb of
          EQ -> case (patcha, patchb) of
            (Just pa, Just pb) -> Just $ compare pa pb
            (Just _, Nothing) -> Just GT
            (Nothing, Just _) -> Just LT
            (Nothing, Nothing) -> Just EQ
          result -> Just result
        result -> Just result
    (_, _) ->
      Nothing

data Status
  = DoesNotExist
  | Inaccessible
  | Unsupported
  | Stopped
  | Started
  | Error Int Text
  deriving (Show)

status :: Cluster -> IO Status
status cluster = do
  env <- clusterEnvironment cluster
  (exitCode, outputRaw) <-
    Process.readProcessInterleaved
      $ Process.setEnv env
      $ Process.proc "pg_ctl" ["status"]
  case exitCode of
    ExitSuccess -> pure Started
    ExitFailure code -> do
      version <- version cluster
      case version `versionCompare` minVersion of
        Nothing ->
          -- The version is unknown so this is a bust.
          pure Unsupported
        Just LT ->
          -- Versions before 9.4 had different rules for what the `pg_ctl
          -- status` exit codes meant, but versions before 9.5 are not
          -- supported upstream so here we just report an error. See
          -- https://www.postgresql.org/support/versioning/.
          pure Unsupported
        Just _ ->
          case code of
            -- 3 = data directory is present and accessible, server not running.
            3 -> pure Stopped
            -- 4 = data directory is not present or is not accessible.
            4 -> do
              dataDirExists <- doesDirectoryExist $ dataDir cluster
              if dataDirExists
                then pure Inaccessible
                else pure DoesNotExist
            -- Everything else is an error.
            _ -> pure $ Error code $ decodeOutput outputRaw
  where
    minVersion =
      Version 9 5 Nothing

clusterEnvironment :: Cluster -> IO [(String, String)]
clusterEnvironment Cluster {dataDir, environ} =
  augmentEnvironment $ environ ++ [("PGDATA", dataDir)]

augmentEnvironment :: [(String, String)] -> IO [(String, String)]
augmentEnvironment overrides = do
  environment <- Environment.getEnvironment
  let overridesDict = Dict.fromList overrides
  let environmentDict = Dict.fromList environment
  let combinedDict = Dict.union overridesDict environmentDict
  pure $ Dict.toList combinedDict

decodeOutput =
  Data.Text.Lazy.toStrict . Data.Text.Lazy.Encoding.decodeUtf8
