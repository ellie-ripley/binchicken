{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scoring where

import Import.NoFoundation
  ( (+)
  , (.)
  , ($)
  , fmap
  , foldl'
  , foldr
  , max
  , otherwise
  , Attempt(..)
  , Bool(..)
  , Entity(..)
  , Eq
  , Functor
  , Int
  , Key(..)
  , Maybe(..)
  , Ord(..)
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
newtype Results a = Results { unResults :: Map ExerciseType a }
  deriving(Functor)

data SummaryRow a = SummaryRow
  { srEmail :: Text
  , srResults :: Results a
  }
  deriving(Functor)

-- | Information about a collection of users' progress
newtype Summary a = Summary { unSummary :: Map (Key User) (SummaryRow a) }
  deriving(Functor)

-- | Results at the start of time
emptyResults :: Results Progress
emptyResults =
  let startRT = Progress 0 0 0
  in  Results $ foldl' (\acc et -> M.insert et startRT acc) M.empty activeExerciseTypes

-- | Given a user, an empty row for that user
emptyRow :: User -> SummaryRow Progress
emptyRow u = SummaryRow
  { srEmail = userEmail u
  , srResults = emptyResults
  }

-- | Given a list of users, an empty summary for those users
emptySumm :: [Entity User] -> Summary Progress
emptySumm [] = Summary M.empty
emptySumm ((Entity i u):eus) =
  let (Summary runningMap) = emptySumm eus
  in case M.lookup i runningMap of
    Nothing -> Summary $ M.insert i (emptyRow u) runningMap
    Just _  -> Summary runningMap

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
  -> SummaryRow Progress
  -> SummaryRow Progress
addAttempt et corr (SummaryRow em res) =
  SummaryRow em newRes
  where
    newRes = Results $ let upd = if corr then correctUpdate else incorrectUpdate
                       in M.adjust upd et (unResults res)

-- | Takes lists of attempts and of users to create summary table
-- | only counts attempts where the user is present in the list of users
-- | assumes attempts sorted newest to oldest (current streak at head)
tally :: [Entity User] -> [(Entity Attempt, Entity Exercise)] -> Summary Progress
tally usrs = foldl' inserter (emptySumm usrs)
  where
    inserter :: Summary Progress -> Entity Attempt -> Summary Progress
    inserter srs ((Entity _ att), (Entity _ ex)) =
      case attemptUserId att of -- does the attempt have a user id?
        Nothing  -> srs
        Just uid -> Summary $ M.adjust
                                (addAttempt (exerciseExerciseType ex) (attemptIsCorrect att))
                                uid
                                (unSummary srs)

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
exScore (SummaryRow _ res) et = M.lookup et (unResults res)

totalPoints :: SummaryRow Int -> Int
totalPoints sr = foldr adder 0 (unResults $ srResults sr)
  where
    adder i = (i +)
