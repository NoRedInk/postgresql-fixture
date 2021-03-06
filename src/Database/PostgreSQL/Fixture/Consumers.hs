module Database.PostgreSQL.Fixture.Consumers
  ( Resource (..),
    Consumer (..),
    ConsumerType (..),
    acquire,
    release,
    inUse,
  )
where

import Control.Exception (catchJust)
import Foreign.C.Error (Errno (Errno), eNOTEMPTY)
import GHC.IO.Exception (IOException (ioe_errno))
import qualified System.Directory as Directory
import System.FilePath ((</>), FilePath)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Process (getProcessID)
import System.Posix.Types (CPid (CPid))
import Text.Printf (printf)

newtype Resource
  = Resource FilePath

data ConsumerType
  = Persistent String
  | Runtime

data Consumer
  = Consumer
      { resource :: Resource,
        consumerType :: ConsumerType
      }

acquire :: Consumer -> IO ()
acquire consumer@Consumer {resource = Resource lockDir} = do
  Directory.createDirectoryIfMissing True lockDir
  lockFilePath_ <- lockFilePath consumer
  writeFile lockFilePath_ ""

release :: Consumer -> IO ()
release consumer =
  catchJust predicate removeLockFile pure
  where
    removeLockFile = do
      lockFilePath_ <- lockFilePath consumer
      Directory.removeFile lockFilePath_
    predicate e
      | isDoesNotExistError e = Just ()
      | otherwise = Nothing

inUse :: Resource -> IO Bool
inUse (Resource lockDir) =
  catchJust predicate removeLockDir pure
  where
    removeLockDir = do
      Directory.removeDirectory lockDir
      pure False
    predicate e
      | isDoesNotExistError e = Just False
      | ioe_errno e == Just eNotEmpty = Just True
      | otherwise = Nothing
    eNotEmpty =
      unwrapErrno eNOTEMPTY
    unwrapErrno (Errno errno) =
      errno

lockFilePath :: Consumer -> IO FilePath
lockFilePath Consumer {resource = Resource lockDir, consumerType} =
  case consumerType of
    Persistent name ->
      pure $ lockDir </> name
    Runtime -> do
      (CPid pid) <- getProcessID
      pure $ lockDir </> printf "pid.%u" pid
