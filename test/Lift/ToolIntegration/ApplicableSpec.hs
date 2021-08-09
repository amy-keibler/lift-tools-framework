{-# LANGUAGE QuasiQuotes #-}
module Lift.ToolIntegration.ApplicableSpec where

import Lift.ToolIntegration.Applicable
import Test.Hspec
import Text.RE.TDFA.Text
import Mock.Project
import Test.HMock
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
    it "should be applicable if the repository has the expected file" $
      ["Cargo.toml", "Cargo.lock", "src/main.rs"] `inTheProjectShouldBe` Applicable
    it "should be applicable if the repository has the expected file in a subfolder" $
      ["rust/Cargo.toml", "rust/Cargo.lock", "rust/src/main.rs"] `inTheProjectShouldBe` Applicable
    it "should not be applicable if the repository lacks the expected file" $
      ["build.gradle", "src/main/java/biz/haha/Factory.java"] `inTheProjectShouldBe` NotApplicable
  describe "Determining if any of multiple conditions are applicable" $ do
    it "should not be applicable if none of the conditions are applicable" $
      applicabilityScenario (MultipleConditions (ApplicableIfFileIsPresent buildGradle :| [ApplicableIfFileIsPresent cargoToml])) NotApplicable  []
    it "should be applicable if any condition is applicable" $
      applicabilityScenario (MultipleConditions (AlwaysApplicable :| [ApplicableIfFileIsPresent cargoToml])) Applicable []

inTheProjectShouldBe :: [Text] -> Applicability -> IO ()
inTheProjectShouldBe = flip $ applicabilityScenario (ApplicableIfFileIsPresent cargoToml)

applicabilityScenario :: ApplicabilityCondition -> Applicability -> [Text] -> IO ()
applicabilityScenario condition expectedApplicability files = runMockT $ do
  expectAny $ ListFiles |-> files
  applicability <- determineApplicability condition
  liftIO $ do
    applicability `shouldBe` expectedApplicability

cargoToml :: RE
cargoToml = [re|Cargo\.toml|]

buildGradle :: RE
buildGradle = [re|build\.gradle|]
