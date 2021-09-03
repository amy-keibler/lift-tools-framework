{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
module Lift.ToolIntegration
  ( module Lift.ToolIntegration.Applicable
  , module Lift.ToolIntegration.Project
  , module Lift.ToolIntegration.Run
  , module Lift.ToolIntegration.ToolResults
  , ToolApplication(..)
  , runToolMain
  ) where

import           Data.Aeson
import qualified Data.Text.IO as T
import           Lift.ToolIntegration.Applicable
import           Lift.ToolIntegration.Cli
import           Lift.ToolIntegration.Log
import           Lift.ToolIntegration.Project
import           Lift.ToolIntegration.Run
import           Lift.ToolIntegration.ToolResults
import           Options.Applicative
import           System.Directory
import           System.Environment (getProgName)
import           System.Log.FastLogger

import           Relude

-- | The configuration for your Haskell application
data ToolApplication = ToolApplication
  { applicabilityCondition :: ApplicabilityCondition
  -- ^ The condition used to determine if a your tool should run on a particular repository
  , runTemplate :: RunTemplate
  -- ^ The template used to produce tool results from the repository
  }

-- | The entry-point for your Haskell application
runToolMain :: (ProjectContext -> ToolApplication) -> IO ()
runToolMain toolApplication = do
  cli <- execParser application
  programName <- getProgName
  outputFile <- getXdgDirectory XdgData programName
  createDirectoryIfMissing True outputFile
  let loggerType = LogFile (FileLogSpec
                             { log_file = outputFile <> "/lift_tool_output.log" 
                             , log_file_size = 1_000_000_000
                             , log_backup_number = 2
                             }) 1024
  timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
  withTimedFastLogger timeCache loggerType (runTool toolApplication cli)

runTool :: (ProjectContext -> ToolApplication) -> Cli -> TimedFastLogger -> IO ()
runTool toolApplicationFromContext Cli{..} fastLogger = do
  let projectContext = ProjectContext { projectRoot = projectFolder
                                      , projectLogger = constructLogger fastLogger configuredLogLevel
                                      }
      ToolApplication {..} = toolApplicationFromContext projectContext
  usingReaderT projectContext $ do
    case cliAction of
      CheckIfApplicable -> applicable applicabilityCondition
      OutputApiVersion -> version
      Run -> run runTemplate

applicable :: (MonadProject m, MonadIO m) => ApplicabilityCondition -> m ()
applicable applicabilityCondition = do
  logDebug "Checking applicability"
  response <- toApiResponse <$> determineApplicability applicabilityCondition
  logDebug $ "Applicable = " <> show response
  outputJson response

version :: (MonadIO m) => m ()
version = outputJson (1 :: Int)

run :: (MonadProject m, MonadIO m) => RunTemplate -> m ()
run runTemplate = executeTemplate runTemplate >>= outputJson

outputJson :: (ToJSON a, MonadIO m) => a -> m ()
outputJson = liftIO . T.putStrLn . decodeUtf8 . encode
