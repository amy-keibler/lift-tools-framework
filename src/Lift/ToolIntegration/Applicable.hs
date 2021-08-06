module Lift.ToolIntegration.Applicable
  ( Applicability (..)
  , CheckApplicability (..)
  , determineApplicability
  , toApiResponse
  ) where

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

data CheckApplicability
  = AlwaysApplicable
  | ApplicableIfFileIsPresent Text
  deriving (Eq, Show)

determineApplicability :: CheckApplicability -> IO Applicability
determineApplicability AlwaysApplicable = pure Applicable
determineApplicability (ApplicableIfFileIsPresent _filePattern) = error "To implement"
