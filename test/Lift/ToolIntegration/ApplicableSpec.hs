module Lift.ToolIntegration.ApplicableSpec where

import Lift.ToolIntegration.Applicable
import Test.Hspec
import Relude

spec :: Spec
spec = do
  describe "Applicability" $ do
    it "should default to not applicable" $
      mempty `shouldBe` NotApplicable
    it "should be applicable if any sub-check is applicable" $ do
      (Applicable <> NotApplicable) `shouldBe` Applicable
      (NotApplicable <> Applicable) `shouldBe` Applicable
  describe "Determining if a given repository is applicable" $ do
    it "should always be applicable when configured as such" $
      determineApplicability AlwaysApplicable `shouldReturn` Applicable
    it "should be applicable if the repository has the expected file" $ do
      determineApplicability (ApplicableIfFileIsPresent "Cargo.toml") `shouldReturn` Applicable
    it "should not be applicable if the repository lacks the expected file" $ do
      determineApplicability (ApplicableIfFileIsPresent "Cargo.toml") `shouldReturn` Applicable
