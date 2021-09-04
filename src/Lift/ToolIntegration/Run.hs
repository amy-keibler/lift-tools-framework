{-# LANGUAGE RecordWildCards #-}
module Lift.ToolIntegration.Run
  ( FileContents(..)
  , ProcessOutputTranslator
  , RunTemplate(..)
  , executeTemplate
  ) where

import Lift.ToolIntegration.Project
import Lift.ToolIntegration.ToolResults
import System.FilePath.Posix (makeRelative)
import Text.RE.TDFA.Text

import Relude

type ProcessOutputTranslator
  =  Text -- ^ The output from executing the process
  -> [ToolResult]

data FileContents = FileContents
  { fileName :: Text
  , fileContent :: Text
  }

data RunTemplate
  = RunProcess
    { processName :: Text
    , processArgs :: [Text]
    , processEnv :: [(Text, Text)]
    , outputToToolResults :: ProcessOutputTranslator
    }
  | RunPerFile
    { fileFilter :: RE
    , fileContentsToToolResults :: FileContents -> [ToolResult]
    }

executeTemplate :: (MonadProject m) => RunTemplate -> m [ToolResult]
executeTemplate RunProcess {..} = do
  projectRoot <- getProjectRoot
  result <- runCommand processName processArgs processEnv
  case result of
    Right r -> pure $ makeFilePathRelative projectRoot <$> outputToToolResults r
    Left e -> do
      logError $ "Template failed with error code: " <> show (code e)
      logDebug $ "Error standard output: " <> output e
      logDebug $ "Error standard error: " <> errorOutput e 
      pure []
executeTemplate RunPerFile {..} = do
  projectRoot <- getProjectRoot
  files <- filter (\f -> matched $ f ?=~ fileFilter) <$> listFiles
  fileContents <- traverse toFileContents files
  pure $ makeFilePathRelative projectRoot <$> foldMap fileContentsToToolResults fileContents
  where
    toFileContents :: MonadProject m => Text -> m FileContents
    toFileContents file = do
      contents <- contentsOfFile file
      pure FileContents
        { fileName = file
        , fileContent = contents
        }


makeFilePathRelative :: Text -> ToolResult -> ToolResult
makeFilePathRelative projectRoot ToolResult {..} = let f = toText $ makeRelative (toString projectRoot) (toString file)
  in ToolResult
    { file = f
    , ..
    }
