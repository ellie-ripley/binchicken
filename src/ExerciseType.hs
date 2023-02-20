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
  | ProofIntuitionistic
  | ProofNormalize
  | CounterexampleIntuitionistic
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
  ProofWithRequirements -> "Provide a proof in NJ that meets certain requirements"
  ProofIntuitionistic -> "Provide a proof of an argument in NJ"
  ProofNormalize -> "Give a proof in NJ and normalize it"
  CounterexampleIntuitionistic -> "Give an intuitionistic counterexample or proof for an argument"

data ExerciseTargets =
  ExerciseTargets { totalMilestone1 :: Int
                  , totalMilestone2 :: Int
                  , streakMilestone1 :: Int
                  }
  deriving (Read, Show, Eq)

