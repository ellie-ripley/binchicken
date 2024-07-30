{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scoring where

import Import.NoFoundation
  ( (+)
  , (.)
  , ($)
  , (<*>)
  , (<$>)
  , fmap
  , foldl'
  , foldr
  , max
  , otherwise
  , Attempt(..)
  , Bool(..)
  , Entity(..)
  , Eq
  , Exercise(..)
  , Functor
  , Int
  , Key(..)
  , Maybe(..)
  , Ord(..)
  , Read
  , Score(..)
  , Show(..)
  , Text
  , User(..)
  )

import Data.Map (Map)
import qualified Data.Map                  as M

import ExerciseType
  ( ExerciseTargets(..)
  , ExerciseType(..)
  )
import Settings.Binchicken (activeExerciseTypes, targets)

-- | Information about a single user's progress on a single exercise type
data Progress =
  Progress { currentStreak :: Int
           , bestStreak    :: Int
           , totalCorrect  :: Int
           }
  deriving (Eq, Read, Show)

-- | Information about a single user's overall progress
newtype Results a = Results { unResults :: Map ExerciseType a }
  deriving(Functor)

data SummaryRow a = SummaryRow
  { srUid :: Key User
  , srEmail :: Maybe Text
  , srResults :: Results a
  }
  deriving(Functor)

-- | Information about a collection of users' progress
newtype Summary a = Summary { unSummary :: Map (Key User) (SummaryRow a) }
  deriving(Functor)

tally_aux :: Score -> Progress
tally_aux sc =
  Progress { currentStreak = scoreCurrentStreak sc
           , bestStreak = scoreBestStreak sc
           , totalCorrect = scoreNumCorrect sc
           }

-- | Adds a new exercise type with progress to a SummaryRow Progress.
-- | Silently overwrites if the ExerciseType is already present.
updateSummaryRow
  :: ExerciseType
  -> Progress
  -> SummaryRow Progress
  -> SummaryRow Progress
updateSummaryRow et pr sr =
  sr { srResults = Results (M.insert et pr (unResults $ srResults sr)) }

-- | Takes a list of rows from the Score table and produces a summary table
-- | results only reasonable if we have either all Score rows or none for each user
tally :: [Entity Score] -> Summary Progress
tally [] = Summary M.empty
tally ((Entity _ sc):xs) =
  Summary
   . M.adjust (updateSummaryRow (scoreExerciseType sc) (tally_aux sc))
              (scoreUserId sc)
   . unSummary $ tally xs

-- TODO: something's not right here. Where does the initial email get filled in, even with Nothing?



-- | Functions for taking Progress and moving to actual scores

calcPoints :: ExerciseTargets -> Progress -> Int
calcPoints (ExerciseTargets tot1 tot2 str) (Progress _ bes tot)
  | bes >= str  = 4
  | tot >= tot2 = 3
  | tot >= tot1 = 2
  | tot >= 1    = 1
  | otherwise   = 0

pointsEarned :: ExerciseType -> Progress -> Int
pointsEarned = calcPoints . targets

calculateResults :: Results Progress -> Results Int
calculateResults = Results . M.mapWithKey pointsEarned . unResults

calculateSummaryRow :: SummaryRow Progress -> SummaryRow Int
calculateSummaryRow sr = sr { srResults = calculateResults (srResults sr) }

calculateSummary :: Summary Progress -> Summary Int
calculateSummary = Summary . fmap calculateSummaryRow . unSummary

exScore :: SummaryRow Int -> ExerciseType -> Maybe Int
exScore (SummaryRow _ _ res) et = M.lookup et (unResults res)

totalPoints :: SummaryRow Int -> Int
totalPoints sr = foldr adder 0 (unResults $ srResults sr)
  where
    adder i = (i +)
