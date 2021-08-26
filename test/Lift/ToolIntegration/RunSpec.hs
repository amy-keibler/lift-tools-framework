{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}
module Lift.ToolIntegration.RunSpec where

import Lift.ToolIntegration.Project
import Lift.ToolIntegration.Run
import Lift.ToolIntegration.ToolResults
import Text.RE.TDFA.Text
import Test.Hspec
import Mock.Project
import Test.HMock
import Relude

spec :: Spec
spec = do
  describe "Run" $ do
    describe "process template" $ do
      it "should make paths relative" $ do
        runPathScenario "/tmp/fake_root/folder/test.txt" "folder/test.txt"
      it "should leave relative paths unchanged" $ do
        runPathScenario "folder/test.txt" "folder/test.txt"
      it "should call the function when the process returned successfully" $
        runProcessScenario
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
        runProcessScenario (Left $ RunFailed { code = 1, output = "failed", errorOutput = "failed" }) (\_ -> error "This should not have been called") []
    describe "per file template" $ do
      it "should exclude unrelated files"
        runPerFileScenarioNoMatchingFiles
      it "should produce tool results for a file"
        runPerFileScenarioMatchingFiles

runProcessScenario :: Either RunCommandError Text -> (Text -> [ToolResult]) -> [ToolResult] -> IO ()
runProcessScenario commandOutput fn expectedResult = runMockT $ do
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
runPathScenario path expectedPath = runProcessScenario
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

runPerFileScenarioNoMatchingFiles :: IO ()
runPerFileScenarioNoMatchingFiles = runMockT $ do
  expect $ GetProjectRoot |-> "/tmp/fake_root"
  expect $ ListFiles |-> [f]
  result <- executeTemplate RunPerFile { fileFilter = [re|.+\.rs|], fileContentsToToolResults  = error "This should not be called because no file should match the regex" }
  liftIO $ do
    result `shouldBe` []
  where
    f :: Text
    f = "test/file.js"

runPerFileScenarioMatchingFiles :: IO ()
runPerFileScenarioMatchingFiles = runMockT $ do
  expectAny $ GetProjectRoot |-> "/tmp/fake_root"
  expect $ ListFiles |-> [f]
  expect $ ContentsOfFile f |-> "file contents"
  result <- executeTemplate RunPerFile { fileFilter = [re|.+\.rs|], fileContentsToToolResults  = toToolResults }
  liftIO $ do
    result `shouldBe` [ToolResult
                        { toolResultType = "test"
                        , message = "file contents"
                        , file = f
                        , line = 1
                        , detailsUrl = Nothing
                        }]
  where
    f :: Text
    f = "test/file.rs"
    toToolResults :: FileContents -> [ToolResult]
    toToolResults FileContents {..} = [ToolResult
                                        { toolResultType = "test"
                                        , message = fileContent
                                        , file = fileName
                                        , line = 1
                                        , detailsUrl = Nothing
                                        }]
