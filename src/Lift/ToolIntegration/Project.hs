{-# LANGUAGE FlexibleInstances #-}
module Lift.ToolIntegration.Project
  ( MonadProject(..)
  , ProjectContext(..)
  ) where

import System.Directory
import System.FilePath.Posix (makeRelative)
import Relude

class (Monad m) => MonadProject m where
  listFiles :: m [Text]

data ProjectContext = ProjectContext
  { projectRoot :: Text
  }

instance (MonadIO m) => MonadProject (ReaderT ProjectContext m) where
  listFiles = do
    folder <- asks projectRoot
    let f = toString folder
    allFiles <- listAllFiles f
    pure $ toText . makeRelative f <$> allFiles

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
