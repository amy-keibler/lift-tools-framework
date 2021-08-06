{-# LANGUAGE RecordWildCards #-}
module Lift.ToolIntegration
  ( module Lift.ToolIntegration.Applicable
  , module Lift.ToolIntegration.ToolResults
  , runToolMain
  ) where

import           Data.Aeson
import           Lift.ToolIntegration.Applicable
import           Lift.ToolIntegration.Cli
import           Lift.ToolIntegration.ToolResults
import           Options.Applicative
import qualified Data.Text.IO as T
import           Relude

-- | The entry-point for your Haskell application
runToolMain :: IO ()
runToolMain = execParser application >>= runTool

runTool :: Cli -> IO ()
runTool Cli{..} = do
  case cliAction of
    CheckIfApplicable -> applicable
    OutputApiVersion -> version
    Run -> run

applicable :: IO ()
applicable = outputJson True

version :: IO ()
version = outputJson (1 :: Int)

run :: IO ()
run = outputJson ([] :: [ToolResult])

outputJson :: (ToJSON a) => a -> IO ()
outputJson = T.putStrLn . decodeUtf8 . encode
