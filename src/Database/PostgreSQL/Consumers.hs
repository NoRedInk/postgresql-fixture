module Database.PostgreSQL.Consumers
  ( Consumer (..),
    ConsumerType (..),
    acquire,
    release,
    inUse,
  )
where

import Control.Exception (catchJust)
import Foreign.C.Error (Errno (Errno), eNOTEMPTY)
import GHC.IO.Exception (IOErrorType (UnsatisfiedConstraints), IOException (ioe_errno))
import qualified System.Directory as Directory
import System.FilePath ((</>), FilePath)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Process (getProcessID)
import System.Posix.Types (CPid (CPid))
import Text.Printf (printf)

data ConsumerType
  = Persistent String
  | Runtime

data Consumer
  = Consumer
      { lockDir :: FilePath,
        consumerType :: ConsumerType
      }

acquire :: Consumer -> IO ()
acquire consumer@Consumer {lockDir, consumerType} = do
  Directory.createDirectoryIfMissing True lockDir
  lockFilePath <- lockFilePath consumer
  writeFile lockFilePath ""

release :: Consumer -> IO ()
release consumer = do
  catchJust predicate removeLockFile pure
  where
    removeLockFile = do
      lockFilePath <- lockFilePath consumer
      Directory.removeFile lockFilePath
    predicate e
      | isDoesNotExistError e = Just ()
      | otherwise = Nothing

inUse :: Consumer -> IO Bool
inUse Consumer {lockDir} =
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
lockFilePath Consumer {lockDir, consumerType} =
  case consumerType of
    Persistent name ->
      pure $ lockDir </> name
    Runtime -> do
      (CPid pid) <- getProcessID
      pure $ lockDir </> printf "pid.%u" pid
