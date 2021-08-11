{-# LANGUAGE QuasiQuotes #-}
module Main where

import Lift.ToolIntegration
import Lift.ToolIntegration.Applicable.Regex
import Relude

application :: ToolApplication
application = ToolApplication
  { applicabilityCondition = ApplicableIfFileIsPresent [re|Cargo\.toml|]
  }

main :: IO ()
main = runToolMain application
