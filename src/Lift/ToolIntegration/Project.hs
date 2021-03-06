{-# LANGUAGE FlexibleInstances #-}
module Lift.ToolIntegration.Project
  ( MonadProject(..)
  , ProjectContext(..)
  , RunCommandError(..)
  , logError
  , logDebug
  , logTrace
  ) where

import Lift.ToolIntegration.Log
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath.Posix (makeRelative)
import qualified System.Process.Typed as TP
import Relude

class (Monad m) => MonadProject m where
  getProjectRoot :: m Text
  listFiles :: m [Text]
  contentsOfFile :: Text -> m Text -- TODO: This should never fail when used correctly, but we should handle that failure correctly when we add error handling (issue #7)
  runCommand :: Text -> [Text] -> [(Text, Text)] -> m (Either RunCommandError Text)
  logMessage :: (ToText message) => LogLevel -> message -> m ()

data RunCommandError
  = RunFailed
    { code :: Int
    , output :: Text
    , errorOutput :: Text
    }
  deriving (Eq, Show)

logError :: (MonadProject m) => Text -> m ()
logError = logMessage Error

logDebug :: (MonadProject m) => Text -> m ()
logDebug = logMessage Debug

logTrace :: (MonadProject m) => Text -> m ()
logTrace = logMessage Trace

data ProjectContext = ProjectContext
  { projectRoot :: Text
  , projectLogger :: LogLevel -> Text -> IO ()
  }

instance (MonadIO m) => MonadProject (ReaderT ProjectContext m) where
  getProjectRoot = asks projectRoot
  listFiles = do
    folder <- asks projectRoot
    logTrace $ "Listing the files in " <> folder
    let f = toString folder
    allFiles <- listAllFiles f
    pure $ toText . makeRelative f <$> allFiles
  contentsOfFile file = do
    folder <- asks projectRoot
    logTrace $ "Retrieving the contents of " <> file <> " in " <> folder
    let filePath = toString $ folder <> "/" <> file
    readFileText filePath
  runCommand exe args env = do
    logTrace $ "Running " <> exe <> " with args " <> show args <> " and env " <> show env
    let process = TP.setEnv (bimap toString toString <$> env)
          $ TP.proc (toString exe) (toString <$> args)
    out <- TP.readProcess process
    pure $ handleProcessOutput out
  logMessage level message = do
    logger <- asks projectLogger
    liftIO $ logger level (toText message)

listAllFiles :: (MonadIO m) => String -> m [String]
listAllFiles folder = do
  contents <- liftIO $ listDirectory (toString folder)
  (folders, files) <- liftIO $ partitionFiles (mappend (folder <> "/") <$> contents)
  subFiles <- join <$> traverse listAllFiles folders
  pure $ files <> subFiles
  where
    partitionFiles :: [String] -> IO ([String], [String])
    partitionFiles = foldlM partitionFile ([], [])
    partitionFile :: ([String], [String]) -> String -> IO ([String], [String])
    partitionFile (folders, files) file = do
      isFile <- doesFileExist file
      isFolder <- doesDirectoryExist file
      pure $ case (isFolder, isFile) of
        (True, False) -> (file : folders, files)
        (False, True) -> (folders , file : files)
        (_, _) -> (folders, files)

handleProcessOutput :: (ExitCode, LByteString, LByteString) -> Either RunCommandError Text
handleProcessOutput (ExitSuccess, o, _) = Right $ decodeUtf8 o
handleProcessOutput (ExitFailure c, o, e) = Left $ RunFailed
  { code = c
  , output = decodeUtf8 o
  , errorOutput = decodeUtf8 e
  }

