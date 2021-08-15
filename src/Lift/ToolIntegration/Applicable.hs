module Lift.ToolIntegration.Applicable
  ( Applicability (..)
  , ApplicabilityCondition (..)
  , determineApplicability
  , toApiResponse
  ) where

import Lift.ToolIntegration.Project
import Text.RE.TDFA.Text (RE, matched, (?=~))

import Relude

data Applicability
  = Applicable
  | NotApplicable
  deriving (Eq, Show)

instance Semigroup Applicability where
  Applicable <> _ = Applicable
  _ <> Applicable = Applicable
  _ <> _ = NotApplicable

instance Monoid Applicability where
  mempty = NotApplicable

toApiResponse :: Applicability -> Bool
toApiResponse Applicable = True
toApiResponse _ = False

data ApplicabilityCondition
  = AlwaysApplicable
  | ApplicableIfFileIsPresent RE
  -- ^ Applicable if a regular expression matches any file in the repository
  | MultipleConditions (NonEmpty ApplicabilityCondition)
  -- ^ Applicable if any of the sub-conditions are applicable

determineApplicability :: (MonadProject m) => ApplicabilityCondition -> m Applicability
determineApplicability AlwaysApplicable = pure Applicable
determineApplicability (MultipleConditions conditions) = fold <$> traverse determineApplicability conditions
determineApplicability (ApplicableIfFileIsPresent filePattern) = foldMap checkFileApplicability <$> listFiles 
  where
    checkFileApplicability :: Text -> Applicability
    checkFileApplicability filepath = if matched $ filepath ?=~ filePattern
      then Applicable
      else NotApplicable
