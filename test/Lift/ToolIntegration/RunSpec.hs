module Lift.ToolIntegration.RunSpec where

import Lift.ToolIntegration.Project
import Lift.ToolIntegration.Run
import Lift.ToolIntegration.ToolResults
import Test.Hspec
import Mock.Project
import Test.HMock
import Relude

spec :: Spec
spec = do
  describe "Run" $ do
    it "should make paths relative" $ do
      runPathScenario "/tmp/fake_root/folder/test.txt" "folder/test.txt"
    it "should leave relative paths unchanged" $ do
      runPathScenario "folder/test.txt" "folder/test.txt"
    it "should call the function when the process returned successfully" $
      runScenario
        (Right "output")
        (\o -> [ToolResult
                   { toolResultType = "test"
                   , message = o
                   , file = "test.txt"
                   , line = 1
                   , detailsUrl = Nothing
                   }])
        [ToolResult
          { toolResultType = "test"
          , message = "output"
          , file = "test.txt"
          , line = 1
          , detailsUrl = Nothing
          }]
    it "should not call the function when an error is returned" $
      runScenario (Left $ RunFailed { code = 1, output = "failed", errorOutput = "failed" }) (\_ -> error "This should not have been called") []

runScenario :: Either RunCommandError Text -> (Text -> [ToolResult]) -> [ToolResult] -> IO ()
runScenario commandOutput fn expectedResult = runMockT $ do
  expect $ GetProjectRoot |-> "/tmp/fake_root/"
  expect $ RunCommand "fake" ["args"] [("fake", "env")] |-> commandOutput
  result <- executeTemplate $ RunProcess
    { processName = "fake"
    , processArgs = ["args"]
    , processEnv = [("fake", "env")]
    , outputToToolResults = fn
    }
  liftIO $ do
    result `shouldBe` expectedResult

runPathScenario :: Text -> Text -> IO ()
runPathScenario path expectedPath = runScenario
  (Right "output")
  (\o -> [ToolResult
           { toolResultType = "test"
           , message = o
           , file = path
           , line = 1
           , detailsUrl = Nothing
           }])
  [ToolResult
    { toolResultType = "test"
    , message = "output"
    , file = expectedPath
    , line = 1
    , detailsUrl = Nothing
    }]
