module Lift.ToolIntegration.Cli
  ( Cli(..)
  , Action(..)
  , application
  ) where

import Options.Applicative
import Relude

data Action
  = OutputApiVersion
  | CheckIfApplicable
  | Run
  deriving (Eq, Show)

data Cli = Cli
  { projectFolder :: Text
  , gitHash :: Text
  , cliAction :: Action
  } deriving (Eq, Show)

application :: ParserInfo Cli
application = info (cli <**> helper)
  (  fullDesc
  <> progDesc "Run a custom tool against a repository in Sonatype Lift"
  )

cli :: Parser Cli
cli = Cli
  <$> argument str (metavar "FOLDER")
  <*> argument str (metavar "GIT_HASH")
  <*> hsubparser
    (  command "applicable" (info applicable (progDesc "Check if the tool is applicable to the repository"))
    <> command "version" (info version (progDesc "Output the version of the API that the tool will use"))
    <> command "run" (info run (progDesc "Run the tool against the repository"))
    )

version :: Parser Action
version = pure OutputApiVersion

applicable :: Parser Action
applicable = pure CheckIfApplicable

run :: Parser Action
run = pure Run
