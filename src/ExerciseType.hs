{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module ExerciseType where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.Persist.TH ( derivePersistField )
import GHC.Generics (Generic)
import Prelude

data ExerciseType =
    DummyExercise
  | IdentifyMainConnective
  | EvaluateBoolean
  | EvaluateStrongKleene
  | EvaluateDunnBelnap
  | CounterexampleClassical
  | CounterexampleNonclassical
  | ProofWithRequirements
  deriving (Eq, Read, Show, Enum, Ord, Bounded, Generic)
instance ToJSON ExerciseType
instance FromJSON ExerciseType
derivePersistField "ExerciseType"

prettyExerciseName :: ExerciseType -> Text
prettyExerciseName = \case
  DummyExercise -> "Dummy exercise"
  IdentifyMainConnective -> "Identify the main connective"
  EvaluateBoolean -> "Evaluate in the Boolean structure"
  EvaluateStrongKleene -> "Evaluate in the strong Kleene structure"
  EvaluateDunnBelnap -> "Evaluate in the Dunn-Belnap structure"
  CounterexampleClassical -> "Give a classical counterexample, if there is one"
  CounterexampleNonclassical -> "Give a counterexample, if there is one, in the indicated matrix"
  ProofWithRequirements -> "Give a proof that meets the given requirements"

data ExerciseTargets =
  ExerciseTargets { totalMilestone1 :: Int
                  , totalMilestone2 :: Int
                  , streakMilestone1 :: Int
                  }
  deriving (Read, Show, Eq)

