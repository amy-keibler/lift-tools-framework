module Main where

import Test.Hspec
import Relude

import qualified Lift.ToolIntegrationSpec as TISpec
import qualified Lift.ToolIntegration.ApplicableSpec as AS

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lift.ToolIntegration" TISpec.spec
  describe "Lift.ToolIntegration.Applicable" AS.spec
