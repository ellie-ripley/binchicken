{-# LANGUAGE NoImplicitPrelude #-}

module Scoring where

import Import.NoFoundation
  ( (+)
  , (.)
  , fmap
  , foldl'
  , foldr
  , fromMaybe
  , map
  , max
  , otherwise
  , repeat
  , zip
  , Attempt(..)
  , Bool(..)
  , Entity(..)
  , Eq
  , Int
  , Key(..)
  , Maybe(..)
  , Ord((>=), (<=))
  , Read
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
type Results = Map ExerciseType Progress

type ResultsCalculated = Map ExerciseType Int

data SummaryRow = SummaryRow
  { srEmail :: Text
  , srResults :: Results
  }

data SummaryRowCalculated = SummaryRowCalculated
  { srcEmail :: Text
  , srcResultsCalculated :: ResultsCalculated
  }

-- | Information about a collection of users' progress
type Summary = Map (Key User) SummaryRow

type SummaryCalculated = Map (Key User) SummaryRowCalculated

-- | Results at the start of time
emptyResults :: Results
emptyResults =
  let startRT = Progress 0 0 0
  in  foldl' (\acc et -> M.insert et startRT acc) M.empty activeExerciseTypes

-- | Given a user, an empty row for that user
emptyRow :: User -> SummaryRow
emptyRow u = SummaryRow
  { srEmail = userEmail u
  , srResults = emptyResults
  }

-- | Given a list of users, an empty summary for those users
emptySumm :: [Entity User] -> Summary
emptySumm [] = M.empty
emptySumm ((Entity i u):eus) =
  let runningS = emptySumm eus
  in case M.lookup i runningS of
    Nothing -> M.insert i (emptyRow u) runningS
    Just _  -> runningS

-- | what one correct answer does to a Progress
correctUpdate :: Progress -> Progress
correctUpdate prog =
  let newCorr = totalCorrect prog + 1
      newCurrStreak = currentStreak prog + 1
      newBestStreak = max newCurrStreak (bestStreak prog)
  in Progress { totalCorrect = newCorr
              , bestStreak = newBestStreak
              , currentStreak = newCurrStreak
              }

-- | what one incorrect answer does to a Progress
incorrectUpdate :: Progress -> Progress
incorrectUpdate prog = prog { currentStreak = 0 }


-- | Adds an attempt to a row, given a Bool saying if the attempt was correct
-- ignores exercise types not already present in the row (in practice, inactives)
addAttempt
  :: ExerciseType
  -> Bool
  -> SummaryRow
  -> SummaryRow
addAttempt et corr sr = sr { srResults = newResults }
  where
    newResults = let upd = if corr then correctUpdate else incorrectUpdate
                 in M.adjust upd et (srResults sr)

-- | Takes full db of attempts and of users to create summary table
-- | only counts attempts where the user is present in the list of users
tally :: [Entity User] -> [Entity Attempt] -> Summary
tally usrs = foldl' inserter (emptySumm usrs)
  where
    inserter :: Summary -> Entity Attempt -> Summary
    inserter srs (Entity _ att) =
      case attemptUserId att of -- does the attempt have a user id?
        Nothing  -> srs
        Just uid -> M.adjust (addAttempt (attemptExerciseType att) (attemptIsCorrect att)) uid srs

-- | Functions for taking Progress and moving to actual scores

pointsEarned :: ExerciseTargets -> Progress -> Int
pointsEarned (ExerciseTargets tot1 tot2 str) (Progress _ bes tot)
  | bes >= str  = 4
  | tot >= tot2 = 3
  | tot >= tot1 = 2
  | tot >= 1    = 1
  | otherwise   = 0

calculateResults :: Results -> ResultsCalculated
calculateResults = M.mapWithKey (pointsEarned . targets)

calculateSRow :: SummaryRow -> SummaryRowCalculated
calculateSRow (SummaryRow em res) = SummaryRowCalculated em (calculateResults res)

calculateSummary :: Summary -> SummaryCalculated
calculateSummary = map calculateSRow


exScore :: SummaryRowCalculated -> ExerciseType -> Maybe Int
exScore src et = M.lookup et (srcResultsCalculated src)

totalPoints :: SummaryRowCalculated -> Maybe Int
totalPoints src = foldr adder (Just 0) (srcResultsCalculated src)
  where
    adder :: Int -> Maybe Int -> Maybe Int
    adder i = fmap (i +)
