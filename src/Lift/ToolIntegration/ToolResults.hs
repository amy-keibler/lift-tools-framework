{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Lift.ToolIntegration.ToolResults
  ( ToolResult(..)
  ) where

import Data.Aeson
import Relude


data ToolResult = ToolResult
  { toolResultType :: Text
  , message :: Text
  , file :: Text
  , line :: Int
  , detailsUrl :: Maybe Text
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
