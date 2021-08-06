{-# LANGUAGE QuasiQuotes #-}
module Lift.ToolIntegrationSpec where

import Data.Aeson
import Lift.ToolIntegration
import Test.Hspec
import Text.RawString.QQ
import Relude

spec :: Spec
spec = do
  describe "Testing the example code" $ do
    it "should default to greeting the world" $ do
      decodeUtf8 (encode
        [ ToolResult
          { toolResultType = "TestTool Result"
          , message = "TestTool Message"
          , file = "TestFile.txt"
          , line = 1
          , detailsUrl = Just "https://example.com"
          }
        ]) `shouldBe` expectedToolNotes

expectedToolNotes :: Text
expectedToolNotes =
  [r|[{"type":"TestTool Result","message":"TestTool Message","file":"TestFile.txt","line":1,"details_url":"https://example.com"}]|]
