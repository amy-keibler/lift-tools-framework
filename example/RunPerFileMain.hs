{-
  This example uses the hadolint and language-docker packages, which are
  licensed as GPLv3.

  As such, this example is provided under those terms as well. A copy of the
  license can be found here (https://www.gnu.org/licenses/gpl-3.0.txt)

  Copyright (C) 2021  Amelia Keibler

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}
module Main where

import Lift.ToolIntegration
import Lift.ToolIntegration.Regex

import Hadolint.Lint
import qualified Hadolint.Rule as HR
import qualified Hadolint.Formatter.Format as HF
import Language.Docker
import Language.Docker.Parser
import Text.Megaparsec.Error

import Relude

application :: ProjectContext -> ToolApplication
application _ = ToolApplication
  { applicabilityCondition = ApplicableIfFileIsPresent [re|Dockerfile|]
  , runTemplate = RunPerFile
    { fileFilter = [re|Dockerfile|]
    , fileContentsToToolResults = processDockerFile
    }
  }

processDockerFile :: FileContents -> [ToolResult]
processDockerFile FileContents {..} = foldMap processResult lintResults
  where
    dockerFileResult :: Either Error Dockerfile
    dockerFileResult = parseText fileContent
    lintResults :: [HF.Result Text DockerfileError]
    lintResults = lint lintOptions [(fileName, dockerFileResult)]

lintOptions :: LintOptions
lintOptions = mempty

processResult :: HF.Result Text DockerfileError -> [ToolResult]
processResult result = toolResultsFromErrors <> toolResultsFromChecks
  where
    toolResultsFromErrors :: [ToolResult]
    toolResultsFromErrors = mconcat $ toList $ translateError (HF.fileName result) <$> (HF.errors result)
    toolResultsFromChecks :: [ToolResult]
    toolResultsFromChecks = toList $ translateFailure (HF.fileName result) <$> (HF.checks result)

translateError :: Text -> ParseErrorBundle Text DockerfileError -> [ToolResult]
translateError f peb = mconcat $ toList $ translateParserError <$> bundleErrors peb
  where
    translateParserError :: ParseError Text DockerfileError -> [ToolResult]
    translateParserError (TrivialError _ _ _) = [ToolResult
                                                { toolResultType = "Dockerfile Parse Error"
                                                , message = f <> " could not be parsed as a valid Dockerfile"
                                                , file = f
                                                , line = 1
                                                , detailsUrl = Nothing
                                                }]
    translateParserError (FancyError _ errors) = translateFancyError <$> toList errors
    translateFancyError :: ErrorFancy DockerfileError -> ToolResult
    translateFancyError (ErrorFail m) = ToolResult
                                        { toolResultType = "Dockerfile Parse Error"
                                        , message = f <> " could not be parsed as a valid Dockerfile: " <> toText m
                                        , file = f
                                        , line = 1
                                        , detailsUrl = Nothing
                                        }
    translateFancyError (ErrorIndentation _ _ _) = ToolResult
                                        { toolResultType = "Dockerfile Parse Error"
                                        , message = f <> " could not be parsed as a valid Dockerfile: Invalid indentation"
                                        , file = f
                                        , line = 1
                                        , detailsUrl = Nothing
                                        }
    translateFancyError (ErrorCustom e) = ToolResult
                                        { toolResultType = "Dockerfile Parse Error"
                                        , message = f <> " could not be parsed as a valid Dockerfile: " <> show e
                                        , file = f
                                        , line = 1
                                        , detailsUrl = Nothing
                                        }
translateFailure :: Text -> HR.CheckFailure -> ToolResult
translateFailure f cf = ToolResult
  { toolResultType = rc
  , message = "# " <> (sev $ HR.severity cf) <> "\n\n" <> HR.message cf
  , file = f
  , line = HR.line cf
  , detailsUrl = Just $ "https://github.com/hadolint/hadolint/wiki/" <> rc
  }
  where
    rc :: Text
    rc = HR.unRuleCode $ HR.code cf
    sev :: HR.DLSeverity -> Text
    sev HR.DLErrorC = "Error"
    sev HR.DLWarningC = "Warning"
    sev HR.DLInfoC = "Info"
    sev HR.DLStyleC = "Style"
    sev HR.DLIgnoreC = "Ignore"

main :: IO ()
main = runToolMain application
