{-# LANGUAGE NumericUnderscores #-}
module Lift.ToolIntegration.Log
  ( LogLevel(..)
  , constructLogger
  , withLogger
  ) where

import System.Directory
import System.Environment (getProgName)
import System.Log.FastLogger

import Relude

data LogLevel
  = Error
  | Debug
  | Trace
  deriving (Eq, Ord, Show)

instance ToText LogLevel where
  toText Error = "Error"
  toText Debug = "Debug"
  toText Trace = "Trace"

instance IsString LogLevel where
  fromString "error" = Error
  fromString "debug" = Debug
  fromString "trace" = Trace
  fromString other = error $ "Expected \"error\", \"debug\", or \"trace\", but got " <> toText other

shouldLog :: LogLevel -> LogLevel -> Bool
shouldLog configuredLevel logStatementLevel = configuredLevel >= logStatementLevel

withLogger :: (TimedFastLogger -> IO ()) -> IO ()
withLogger action = do
  programName <- getProgName
  outputFile <- getXdgDirectory XdgData programName
  createDirectoryIfMissing True outputFile
  let loggerType = LogFile (FileLogSpec
                             { log_file = outputFile <> "/lift_tool_output.log" 
                             , log_file_size = 1_000_000_000
                             , log_backup_number = 2
                             }) 1024
  timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
  withTimedFastLogger timeCache loggerType action

constructLogger :: TimedFastLogger -> LogLevel -> LogLevel -> Text -> IO ()
constructLogger logTarget configuredLevel logStatementLevel msg =
  if shouldLog configuredLevel logStatementLevel
  then logTarget (\time -> toLogStr $ decodeUtf8 time <> " [" <> toText logStatementLevel <> "] " <> toText msg <> "\n")
  else pure ()
