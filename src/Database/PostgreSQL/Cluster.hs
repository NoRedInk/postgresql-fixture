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
    versionCompare,
  )
where

import Data.Attoparsec.Text as Attoparsec
import qualified Data.Map.Strict as Dict
import Data.Text (Text)
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Database.PostgreSQL.Fixture.Settings as Settings
import System.Directory (removeDirectoryRecursive)
import qualified System.Environment as Environment
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
  deriving (Show)

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

version :: Cluster -> IO (Maybe Version)
version cluster = do
  env <- clusterEnvironment cluster
  outputRaw <-
    Process.readProcessStdout_
      $ Process.setEnv env
      $ Process.proc "pg_ctl" ["--version"]
  let output = Data.Text.Lazy.Encoding.decodeUtf8 outputRaw
  let version = parseOnly versionLineParser $ Data.Text.Lazy.toStrict output
  pure $ case version of
    Left err -> Nothing
    Right vn -> Just vn

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
