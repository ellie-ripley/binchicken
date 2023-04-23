{-# LANGUAGE NoImplicitPrelude #-}

module Scoring where

import Import.NoFoundation
  ( (||)
  , otherwise
  , Bool
  , Enum
  , Eq
  , Int
  , Ord((>=))
  , Read
  , Show
  )

import ExerciseType
  ( ExerciseTargets(..)
  )


data Progress =
  Progress { currentStreak :: Int
           , bestStreak    :: Int
           , totalCorrect  :: Int
           }
  deriving (Eq, Read, Show)

data TargetsReached =
  TargetsReached { target1 :: Bool
                 , target2 :: Bool
                 , target3 :: Bool
                 , target4 :: Bool
                 }

targetsReached :: ExerciseTargets -> Progress -> TargetsReached
targetsReached (ExerciseTargets t1 t2 s1) (Progress _ bes tot) =
  TargetsReached (tot >= 1  || bes >= s1)
                 (tot >= t1 || bes >= s1)
                 (tot >= t2 || bes >= s1)
                 (bes >= s1)

data PointsEarned =
    ZeroPoints
  | OnePoint
  | TwoPoints
  | ThreePoints
  | FourPoints
  deriving (Eq, Ord, Enum)

pointsEarned :: ExerciseTargets -> Progress -> PointsEarned
pointsEarned et pr
  | tr4 = FourPoints
  | tr3 = ThreePoints
  | tr2 = TwoPoints
  | tr1 = OnePoint
  | otherwise = ZeroPoints
  where (TargetsReached tr1 tr2 tr3 tr4) = targetsReached et pr
