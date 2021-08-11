{-# LANGUAGE RecordWildCards #-}
module Lift.ToolIntegration
  ( module Lift.ToolIntegration.Applicable
  , module Lift.ToolIntegration.Project
  , module Lift.ToolIntegration.ToolResults
  , ToolApplication(..)
  , runToolMain
  ) where

import           Data.Aeson
import           Lift.ToolIntegration.Applicable
import           Lift.ToolIntegration.Cli
import           Lift.ToolIntegration.Project
import           Lift.ToolIntegration.ToolResults
import           Options.Applicative
import qualified Data.Text.IO as T
import           Relude

-- | The configuration for your Haskell application
data ToolApplication = ToolApplication
  { applicabilityCondition :: ApplicabilityCondition
  }

-- | The entry-point for your Haskell application
runToolMain :: ToolApplication -> IO ()
runToolMain toolApplication = execParser application >>= runTool toolApplication

runTool :: ToolApplication -> Cli -> IO ()
runTool ToolApplication{..} Cli{..} = do
  let projectContext = ProjectContext { projectRoot = projectFolder }
  usingReaderT projectContext $ do
    case cliAction of
      CheckIfApplicable -> applicable applicabilityCondition
      OutputApiVersion -> version
      Run -> run

applicable :: (MonadProject m, MonadIO m) => ApplicabilityCondition -> m ()
applicable applicabilityCondition = do
  response <- toApiResponse <$> determineApplicability applicabilityCondition
  outputJson response

version :: (MonadIO m) => m ()
version = outputJson (1 :: Int)

run :: (MonadIO m) => m ()
run = outputJson ([] :: [ToolResult])

outputJson :: (ToJSON a, MonadIO m) => a -> m ()
outputJson = liftIO . T.putStrLn . decodeUtf8 . encode
