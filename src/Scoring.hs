{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scoring where

import Import.NoFoundation
  ( (+)
  , (.)
  , ($)
  , (==)
  , (<>)
  , divMod
  , error
  , fmap
  , foldr
  , fromIntegral
  , fst
  , map
  , otherwise
  , pack
  , show
  , Bool(..)
  , Entity(..)
  , Eq
  , Functor
  , Int
  , Key(..)
  , Maybe(..)
  , Ord(..)
  , Read
  , Score(..)
  , Show(..)
  , User(..)
  )

import Data.Ratio (Rational, (%), denominator, numerator)
import Data.Map (Map)
import qualified Data.Map                  as M
import Data.Text (Text)

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

data Correct = Correct | Incorrect
  deriving (Eq, Read, Show)

boolToCorrect :: Bool -> Correct
boolToCorrect True  = Correct
boolToCorrect False = Incorrect

correctToBool :: Correct -> Bool
correctToBool Correct = True
correctToBool Incorrect = False

-- | Progress before anything is done
zeroProgress :: Progress
zeroProgress = Progress 0 0 0

-- | updates Progress in light of a correct or incorrect answer
updateProgress
  :: Correct
  -> Progress
  -> Progress
updateProgress Correct p =
  p { currentStreak = (currentStreak p) + 1
    , bestStreak = if (currentStreak p) + 1 > (bestStreak p)
                    then (currentStreak p) + 1
                    else (bestStreak p)
    , totalCorrect = (totalCorrect p) + 1
    }
updateProgress Incorrect p =
  p { currentStreak = 0 }

-- | Information about a single user's overall progress
newtype Results a = Results { unResults :: Map ExerciseType a }
  deriving(Functor)

blankResults :: Results Progress
blankResults = Results (M.fromList $ map (\et -> (et, zeroProgress)) activeExerciseTypes)

data SummaryRow a = SummaryRow
  { srUid :: Key User
  , srEmail :: Text
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

userPair :: Entity User -> (Key User, Text)
userPair (Entity uid u) = (uid, userEmail u)

-- | Generates a SummaryRow for a user, with no results recorded
blankSummaryRow
  :: Entity User
  -> SummaryRow Progress
blankSummaryRow u =
  let (k, me) = userPair u
  in  SummaryRow k me blankResults

-- | Takes a list of user/email pairs and generates a Summary for all users with no results
blankSummary
  :: [Entity User]
  -> Summary Progress
blankSummary [] = Summary M.empty
blankSummary (u:usrs) = Summary $ M.insert (fst $ userPair u) (blankSummaryRow u) (unSummary $ blankSummary usrs)

-- | Takes lists of user/email pairs and rows from the Score table and produces a summary table
-- | results only reasonable if we have either all Score rows or none for each user
tally
  :: [Entity User] -- user/email pairs
  -> [Entity Score]     -- Score rows
  -> Summary Progress
tally usrs [] = blankSummary usrs
tally usrs ((Entity _ sc):xs) =
  Summary
   . M.adjust (updateSummaryRow (scoreExerciseType sc) (tally_aux sc))
              (scoreUserId sc)
   . unSummary $ tally usrs xs




-- | Functions for taking Progress and moving to actual scores
data Milestone
  = NoMilestone
  | SmallTotal
  | LargeTotal
  | Streak
  deriving (Eq, Ord, Show)

calcMilestone :: ExerciseTargets -> Progress -> Milestone
calcMilestone (ExerciseTargets tot1 tot2 str) (Progress _ bes tot)
  | bes >= str  = Streak
  | tot >= tot2 = LargeTotal
  | tot >= tot1 = SmallTotal
  | otherwise   = NoMilestone

calcPoints :: Milestone -> Int
calcPoints = \case
  NoMilestone -> 0
  SmallTotal  -> 2
  LargeTotal  -> 3
  Streak      -> 4

pointsEarned :: ExerciseType -> Progress -> Int
pointsEarned et = calcPoints . calcMilestone (targets et)

calculateResults :: Results Progress -> Results Int
calculateResults = Results . M.mapWithKey pointsEarned . unResults

calculateSummaryRow :: SummaryRow Progress -> SummaryRow Int
calculateSummaryRow sr = sr { srResults = calculateResults (srResults sr) }

calculateSummary :: Summary Progress -> Summary Int
calculateSummary = Summary . fmap calculateSummaryRow . unSummary

exScore :: SummaryRow Int -> ExerciseType -> Maybe Int
exScore (SummaryRow _ _ res) et = M.lookup et (unResults res)

totalPoints :: SummaryRow Int -> Rational
totalPoints sr = foldr adder 0 (unResults $ srResults sr)
  where
    adder n a = a + (fromIntegral n)

displayPoints :: Rational -> Text
displayPoints r
  | denominator r == 1 = pack . show $ numerator r
  | otherwise = "There's been a problem in the scoring; please report this error!"
