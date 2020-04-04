module Database.PostgreSQL.Fixture.Util
  ( augmentEnvironment,
    temporaryDirectory,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Acquire
import qualified Data.Map.Strict as Dict
import qualified System.Directory
import qualified System.Environment as Environment
import System.FilePath ((</>))
import qualified System.Posix.Temp

augmentEnvironment :: [(String, String)] -> IO [(String, String)]
augmentEnvironment overrides = do
  environment <- Environment.getEnvironment
  let overridesDict = Dict.fromList overrides
  let environmentDict = Dict.fromList environment
  let combinedDict = Dict.union overridesDict environmentDict
  pure $ Dict.toList combinedDict

temporaryDirectory :: Data.Acquire.Acquire FilePath
temporaryDirectory = do
  -- NOTE: keep the temporary path short because UNIX domain socket paths are
  -- limited to 103 bytes on macOS (maybe it's different on Linux) and macOS
  -- likes to set $TMP to /var/folders/83/p3y5dr1s0zj0w95s7t_mzwcc0000gk/T/ or
  -- something bizarre like that.
  tmp <- liftIO System.Directory.getTemporaryDirectory
  Data.Acquire.mkAcquire
    (System.Posix.Temp.mkdtemp (tmp </> "pg."))
    System.Directory.removeDirectoryRecursive
