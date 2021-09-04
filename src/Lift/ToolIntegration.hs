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
import           System.Log.FastLogger (TimedFastLogger)

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
  withLogger (runTool toolApplication cli)

runTool :: (ProjectContext -> ToolApplication) -> Cli -> TimedFastLogger -> IO ()
runTool toolApplicationFromContext Cli{..} fastLogger = do
  let projectContext = ProjectContext { projectRoot = projectFolder
                                      , projectLogger = constructLogger fastLogger configuredLogLevel
                                      }
      ToolApplication {..} = toolApplicationFromContext projectContext
  usingReaderT projectContext $ do
    logDebug $ "Project: " <> projectFolder
    case cliAction of
      CheckIfApplicable -> applicable applicabilityCondition
      OutputApiVersion -> version
      Run -> run runTemplate

applicable :: (MonadProject m, MonadIO m) => ApplicabilityCondition -> m ()
applicable applicabilityCondition = do
  logDebug "Started Checking Applicability"
  response <- toApiResponse <$> determineApplicability applicabilityCondition
  outputJson response
  logDebug $ "Applicable = " <> show response
  logDebug "Completed Checking Applicability"

version :: (MonadIO m) => m ()
version = outputJson (1 :: Int)

run :: (MonadProject m, MonadIO m) => RunTemplate -> m ()
run runTemplate = do
  logDebug "Started Running"
  executeTemplate runTemplate >>= outputJson
  logDebug "Completed Running"

outputJson :: (ToJSON a, MonadIO m) => a -> m ()
outputJson = liftIO . T.putStrLn . decodeUtf8 . encode
