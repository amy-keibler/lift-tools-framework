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
    around withTempFiles $
      describe "listFiles" $ do
        it "should list all files in a project" $ \folder -> do
          writeFile (toString folder <> "/topLevelFile.txt") ""
          createDirectory (toString folder <> "/folder/")
          writeFile (toString folder <> "/folder/secondLevelFile.log") ""
          runProject folder `shouldReturn` ["topLevelFile.txt", "folder/secondLevelFile.log"]

runProject :: Text -> IO [Text]
runProject folder = let project = ProjectContext { projectRoot = folder }
                    in usingReaderT project listFiles

withTempFiles :: (Text -> IO ()) -> IO ()
withTempFiles = bracket createFiles deleteFiles

createFiles :: IO Text
createFiles = toText <$> createTempDirectory "/tmp/" "lift-tools-framework-project-spec"

deleteFiles :: Text -> IO ()
deleteFiles = removeDirectoryRecursive . toString
