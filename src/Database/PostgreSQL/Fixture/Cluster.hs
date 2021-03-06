{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Fixture.Cluster
  ( Cluster (..),
    create,
    start,
    stop,
    destroy,
    Status (..),
    status,
    lockShared,
    lockExclusive,
    version,
  )
where

import qualified Data.ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Database.PostgreSQL.Fixture.Settings as Settings
import Database.PostgreSQL.Fixture.Util (augmentEnvironment)
import Database.PostgreSQL.Fixture.Version (Version (Version), versionCompare, versionLineParse)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import qualified System.FileLock as FileLock
import System.FilePath ((</>), FilePath, takeBaseName, takeDirectory)
import qualified System.Posix.User as User
import qualified System.Process.Typed as Process
import Text.Printf (printf)

-- TODO: Split out cluster stuff from "where are the PostgreSQL binaries" stuff.
--   Some of the code below is bothered about checking the version of PostgreSQL
--   tools that we're using, but that's kind of a separate concern. Perhaps we
--   should pass in here a `Tools` structure that determines what tools we use
--   to carry out the tasks herein. Separately we ought to be able to inspect
--   what version a preexisting cluster can be run with (see the `PG_VERSION`
--   file in $PGDATA), and that _is_ a task for this module.

data Cluster
  = Cluster
      -- TODO: Ensure that `dataDir` is absolute. The `-k dir` argument when
      -- starting must be absolute (and we use `dataDir` for this right now) or
      -- else the cluster will not start.
      { dataDir :: FilePath,
        environ :: [(String, String)]
      }
  deriving (Show)

create :: Cluster -> IO ()
create cluster = do
  -- TODO: Set TZ here too? See initdb(1).
  -- TODO: Set locale too? See initdb(1).
  -- TODO: Set --nosync for speed? See initdb(1).
  -- TODO: Just use initdb(1) here instead of pg_ctl(1)? pg_ctl adds almost
  --   nothing for init, and its naïve quoting of arguments seals the deal
  --   against it.
  env <- clusterEnvironment cluster
  Process.runProcess_
    $ Process.setEnv env
    $ Process.proc
      "pg_ctl"
      [ "init",
        "--silent", -- No informational messages.
        "-o",
        "--auth trust --encoding utf8"
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
        -- See all "Note on handling of options" below before adding new options
        -- to this list, especially those that propagate user-supplied settings.
        --
      ]

start :: Cluster -> IO Settings.ConnectionSettings
start cluster@Cluster {dataDir} = do
  -- TODO: Use -o -F to disable fsync for speed? See postgres(1).
  --       Or set `fsync = off` in postgresql.conf.
  -- TODO: Just use postgres(1) here instead of pg_ctl(1)? pg_ctl adds some PID
  --   file stuff (not sure if that's necessary to be honest), some setsid(3)
  --   stuff (easy to reproduce), and can wait for the server to start (useful,
  --   not so easy to reproduce, but not impossible). Its naïve quoting of
  --   arguments, however, leaves a lot to be desired.
  env <- clusterEnvironment cluster
  Process.runProcess_
    $ Process.setEnv env
    $ Process.proc
      "pg_ctl"
      [ "start",
        "--silent", -- No informational messages.
        "-w", -- Wait for start (this is *not* the default).
        "--log=" <> dataDir </> "log", -- Log to `$dataDir/log`.
        "-o",
        -- TODO: Set the socket directory (the argument to `-k`) to something
        -- like ~/.local/share/postgresql/sockets/fhef53 and put a symlink in
        -- the cluster directory to it? Nix can't cope with UNIX sockets and we
        -- can't get it to ignore them but it doesn't mind _symlinks_ to socket
        -- files, so we can put the sockets somewhere Nix isn't going to see
        -- them. There should be a symlink from $PGDATA/sockets (or something
        -- like that) to ~/.local/.../sockets, *and* a symlink from the latter
        -- to $PGDATA. This lets us figure out stale sockets and automatically
        -- reap them. Or maybe put them in $TMP/somewhere in the hope that the
        -- OS will clear them out after a reboot.
        printf "-h '' -p %u -k %s" arbitraryPort dataDir
        --
        -- Explanation of the options being passed in via `-o`:
        --
        -- -h ''   -> Disable all TCP connections.
        --
        -- -p NNNN -> Listen on port NNNN. Because we're only using UNIX
        --            sockets, this is actually the suffix used in the socket
        --            file name, i.e. in `.s.PGSQL.NNNN`.
        --
        -- -k dir..-> The directory in which to put the UNIX socket. Note that
        --            we don't shell-escape the `dataDir` directory we pass to
        --            the `-k` argument.
        --
        -- Note on handling of options (`-o`):
        --
        --   pg_ctl handles options specified via the `-o` flag a little poorly.
        --   They are passed almost as they are to a system(3) call. That means
        --   we could shell escape the data directory for example, but in some
        --   paths through pg_ctl's code these options are wrapped in double
        --   quotes, so shell escaping them here might make things worse. I
        --   haven't picked through pg_ctl's code to figure out exactly what's
        --   safe and what's not so, for now, consider this mechanism kind of
        --   broken – along with the PGOPTIONS environment variable – and follow
        --   a simple rule: make sure your data directory path contains only
        --   printable ASCII, no characters that might be interpreted by the
        --   shell, and no whitespace.
        --
        -- See postgres(1) for more information on these flags.
        --
        -- The parameters above can also be set in $PGDATA/postgresql.conf. See
        -- https://www.postgresql.org/docs/9.6/config-setting.html. Parameters
        -- can also be set using ALTER SYSTEM; this may get around quoting
        -- issues. See https://www.postgresql.org/docs/9.6/sql-altersystem.html.
        --
        -- -h can also be configured by setting `listen_addresses` parameter.
        --
        -- -p can also be set via the `port` parameter or PGPORT environment
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
  -- TODO: Don't use pg_ctl(1) here? pg_ctl reads postmaster.pid and waits for
  --   shutdown to complete but adds little else for our purposes (it has a
  --   "smart" shutdown mode which detects if a backup is in progress and sends
  --   a SIGTERM instead of SIGINT, but that's it, and we're using the "fast"
  --   mode anyway since we don't expect backups of a development database).
  env <- clusterEnvironment cluster
  Process.runProcess_
    $ Process.setEnv env
    $ Process.proc
      "pg_ctl"
      [ "stop",
        "--silent", -- No informational messages.
        "-w", -- Wait for shutdown (default).
        "--mode=fast" -- Shutdown mode (default).
      ]

destroy :: Cluster -> IO ()
destroy Cluster {dataDir} =
  removeDirectoryRecursive dataDir

version :: Cluster -> IO Version
version cluster = do
  env <- clusterEnvironment cluster
  stdout <-
    Process.readProcessStdout_
      $ Process.setEnv env
      $ Process.proc "pg_ctl" ["--version"]
  pure $ versionLineParse $ decodeOutput stdout

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
      version_ <- version cluster
      case version_ `versionCompare` minVersion of
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

lockShared :: Cluster -> IO a -> IO a
lockShared cluster io =
  FileLock.withFileLock (lockFilePath cluster) FileLock.Shared (const io)

lockExclusive :: Cluster -> IO a -> IO a
lockExclusive cluster io =
  FileLock.withFileLock (lockFilePath cluster) FileLock.Exclusive (const io)

lockFilePath :: Cluster -> FilePath
lockFilePath Cluster {dataDir} =
  takeDirectory dataDir </> ("." <> takeBaseName dataDir <> ".lock")

clusterEnvironment :: Cluster -> IO [(String, String)]
clusterEnvironment Cluster {dataDir, environ} =
  augmentEnvironment $ environ ++ [("PGDATA", dataDir)]

decodeOutput :: Data.ByteString.Lazy.ByteString -> Text
decodeOutput =
  Data.Text.Lazy.toStrict . Data.Text.Lazy.Encoding.decodeUtf8
