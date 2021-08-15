{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Lift.ToolIntegration.ToolResults
  ( ToolResult(..)
  ) where

import Data.Aeson
import Relude


data ToolResult = ToolResult
  { toolResultType :: Text
  -- ^ This is used to group tool results in Lift
  , message :: Text
  -- ^ The actionable feedback about the issue your tool found
  , file :: Text
  -- ^ The file that contained the issue
  , line :: Int
  -- ^ The line number of the issue
  , detailsUrl :: Maybe Text
  -- ^ An optional URL that provides additional data about the issue
  } deriving (Eq, Show, Generic)

instance ToJSON ToolResult where
  toJSON ToolResult{..} = object
    [ "type" .= toolResultType
    , "message" .= message
    , "file" .= file
    , "line" .= line
    , "details_url" .= detailsUrl
    ]
  toEncoding ToolResult{..} = pairs
    (  "type" .= toolResultType
    <> "message" .= message
    <> "file" .= file
    <> "line" .= line
    <> "details_url" .= detailsUrl
    )
