module Lift.ToolIntegration.Log
  ( LogLevel(..)
  , constructLogger
  ) where
import System.Log.FastLogger (TimedFastLogger, toLogStr)

import Relude

data LogLevel
  = Error
  | Debug
  | Trace
  deriving (Eq, Ord, Show)

instance ToText LogLevel where
  toText Error = "ERROR"
  toText Debug = "DEBUG"
  toText Trace = "TRACE"

instance IsString LogLevel where
  fromString "error" = Error
  fromString "debug" = Debug
  fromString "trace" = Trace
  fromString other = error $ "Expected \"error\", \"debug\", or \"trace\", but got " <> toText other

shouldLog :: LogLevel -> LogLevel -> Bool
shouldLog configuredLevel logStatementLevel = configuredLevel >= logStatementLevel

constructLogger :: TimedFastLogger -> LogLevel -> LogLevel -> Text -> IO ()
constructLogger logTarget configuredLevel logStatementLevel msg =
  if shouldLog configuredLevel logStatementLevel
  then logTarget (\time -> toLogStr $ decodeUtf8 time <> " [" <> toText logStatementLevel <> "] " <> toText msg <> "\n")
  else pure ()
