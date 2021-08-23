{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}
module Main where

import Data.Aeson
import Lift.ToolIntegration
import Lift.ToolIntegration.Applicable.Regex
import Relude

application :: ProjectContext -> ToolApplication
application ProjectContext{..} = ToolApplication
  { applicabilityCondition = ApplicableIfFileIsPresent [re|tsconfig\.json|]
  , runTemplate = RunProcess
    { processName = "npx"
    , processArgs = [ "tslint", "--force", "--format", "json", "--project", projectRoot]
    , processEnv = [("PATH", "/usr/bin/:./node_modules/.bin")]
    , outputToToolResults = processOutput
    }
  }

data TsLintIssue = TsLintIssue
  { failure :: Text
  , name :: Text
  , ruleName :: Text
  , startPosition :: Position
  } deriving (Generic, FromJSON)

data Position = Position
  { position :: Int
  } deriving (Generic, FromJSON)

processOutput :: Text -> [ToolResult]
processOutput output = let
  decoded = eitherDecode $ encodeUtf8 output
  in
    case decoded of
      Left e -> error (toText e)
      Right issues -> toToolResult <$> issues

toToolResult :: TsLintIssue -> ToolResult
toToolResult TsLintIssue {..} = ToolResult
  { toolResultType = ruleName
  , message = failure
  , file = name
  , line = position startPosition
  , detailsUrl = Nothing
  }

main :: IO ()
main = runToolMain application
