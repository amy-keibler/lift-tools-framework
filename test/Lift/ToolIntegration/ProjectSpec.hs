module Lift.ToolIntegration.ProjectSpec where

import Lift.ToolIntegration.Project
import GHC.IO (bracket)
import Test.Hspec
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory)
import Relude

spec :: Spec
spec = do
  describe "MonadProject" $
    around withTempFiles $ do
      describe "listFiles" $ do
        it "should list all files in a project" $ \folder -> do
          writeFile (toString folder <> "/topLevelFile.txt") ""
          createDirectory (toString folder <> "/folder/")
          writeFile (toString folder <> "/folder/secondLevelFile.log") ""
          listFilesInProject folder `shouldReturn` ["topLevelFile.txt", "folder/secondLevelFile.log"]
      describe "runCommand" $ do
        it "should execute a process with arguments" $ \folder -> do
          runCommandInProject "echo" ["Hello world!"] [] folder `shouldReturn` (Right "Hello world!\n")
        it "should execute a process with an environment" $ \folder -> do
          runCommandInProject "env" [] [("TEST", "VERY_YES")] folder `shouldReturn` (Right "TEST=VERY_YES\n")
        it "should execute a process that fails" $ \folder -> do
          runCommandInProject "ls" ["/no/such/file"] [] folder `shouldReturn` (Left $ RunFailed { code = 2, output = "", errorOutput = "ls: cannot access '/no/such/file': No such file or directory\n" })

listFilesInProject :: Text -> IO [Text]
listFilesInProject folder = let project = ProjectContext { projectRoot = folder }
                    in usingReaderT project listFiles

runCommandInProject :: Text -> [Text] -> [(Text, Text)] -> Text -> IO (Either RunCommandError Text)
runCommandInProject exe args env folder = let project = ProjectContext { projectRoot = folder }
                    in usingReaderT project (runCommand exe args env)

withTempFiles :: (Text -> IO ()) -> IO ()
withTempFiles = bracket createFiles deleteFiles

createFiles :: IO Text
createFiles = toText <$> createTempDirectory "/tmp/" "lift-tools-framework-project-spec"

deleteFiles :: Text -> IO ()
deleteFiles = removeDirectoryRecursive . toString
