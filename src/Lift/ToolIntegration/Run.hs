{-# LANGUAGE RecordWildCards #-}
module Lift.ToolIntegration.Run
  ( OutputTranslator
  , RunTemplate(..)
  , executeTemplate
  ) where

import Lift.ToolIntegration.Project
import Lift.ToolIntegration.ToolResults
import System.FilePath.Posix (makeRelative)

import Relude

type OutputTranslator
  =  Text -- ^ The output from executing the process
  -> [ToolResult]

data RunTemplate
  = RunProcess
    { processName :: Text
    , processArgs :: [Text]
    , processEnv :: [(Text, Text)]
    , outputToToolResults :: OutputTranslator
    }

executeTemplate :: (MonadProject m) => RunTemplate -> m [ToolResult]
executeTemplate RunProcess {..} = do
  projectRoot <- getProjectRoot
  result <- runCommand processName processArgs processEnv
  case result of
    Right r -> pure $ makeFilePathRelative projectRoot <$> outputToToolResults r
    Left _ -> pure [] -- TODO: log errors

makeFilePathRelative :: Text -> ToolResult -> ToolResult
makeFilePathRelative projectRoot ToolResult {..} = let f = toText $ makeRelative (toString projectRoot) (toString file)
  in ToolResult
    { file = f
    , ..
    }
