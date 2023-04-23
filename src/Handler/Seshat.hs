{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Seshat where

import Foundation ( Handler )
import Import.NoFoundation ( Attempt(..)
                           , Entity(..)
                           , Html
                           , Key(..)
                           , User(..)
                           , Yesod(..)
                           , runDB
                           , selectList
                           , setTitle
                           , widgetFile
                           )
import Database.Esqueleto.Legacy (BackendKey(..))

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text, pack)

import ExerciseType (ExerciseType(..), ExerciseTargets(..))
import Settings.Binchicken (activeExerciseTypes, targets)

data RunningTotal = RunningTotal
  { rtCorrect :: Int
  , rtBestStreak :: Int
  , rtCurrentStreak :: Int
  }

displayRunningTotal :: RunningTotal -> Text
displayRunningTotal rt =
  pack (show $ rtCorrect rt)
  <> ", "
  <> pack (show $ rtBestStreak rt)
  <> ", "
  <> pack (show $ rtCurrentStreak rt)

data SummaryRow = SummaryRow
  { srEmail :: Text
  , srResults :: Map ExerciseType RunningTotal
  }

type Summary = Map (Key User) SummaryRow

exScore :: ExerciseType -> Map ExerciseType RunningTotal -> Int
exScore et reslts = case M.lookup et reslts of
  Just rt
    | rtBestStreak rt >= streakMilestone1 (targets et) -> 3
    | rtCorrect rt >= totalMilestone2 (targets et) -> 2
    | rtCorrect rt >= totalMilestone1 (targets et) -> 1
    | otherwise -> 0
  Nothing -> 0

totalScore :: Map ExerciseType RunningTotal -> Int
totalScore reslts = sum $ map (\et -> exScore et reslts) activeExerciseTypes

-- | We'll tally up whatever we find, but we need to make sure we have something for all active exercise types
emptyResults :: Map ExerciseType RunningTotal
emptyResults =
  let startRT = RunningTotal 0 0 0
  in  foldl' (\acc et -> M.insert et startRT acc) M.empty activeExerciseTypes

initRow :: User -> SummaryRow
initRow u = SummaryRow
  { srEmail = userEmail u
  , srResults = emptyResults
  }

displayUserId :: Key User -> String
displayUserId = show . unSqlBackendKey . unUserKey

-- | Given a list of users, creates an empty summary for those users
initSumm :: [Entity User] -> Summary
initSumm [] = M.empty
initSumm ((Entity i u):eus) =
  let runningS = initSumm eus
  in case M.lookup i runningS of
    Nothing -> M.insert i (initRow u) runningS
    Just _  -> runningS

rtCorrectUpdate :: RunningTotal -> RunningTotal
rtCorrectUpdate rt =
  let newCorr = rtCorrect rt + 1
      newCurrStreak = rtCurrentStreak rt + 1
      newBestStreak = if newCurrStreak > rtBestStreak rt
                         then newCurrStreak
                         else rtBestStreak rt
  in RunningTotal { rtCorrect = newCorr
                  , rtBestStreak = newBestStreak
                  , rtCurrentStreak = newCurrStreak
                  }

rtIncorrectUpdate :: RunningTotal -> RunningTotal
rtIncorrectUpdate rt = rt { rtCurrentStreak = 0 }


-- | Adds an attempt to a row, given a Bool saying if the attempt was correct
-- | ignores exercise types not already present in the row (in practice, inactives)
addAttempt
  :: ExerciseType
  -> Bool
  -> SummaryRow
  -> SummaryRow
addAttempt et corr sr = sr { srResults = newResults }
  where
    newResults = let upd = if corr then rtCorrectUpdate else rtIncorrectUpdate
                 in M.adjust upd et (srResults sr)

-- | Takes full db of attempts and of users to create summary table
-- | only counts attempts where the user is present in the list of users
tally :: [Entity User] -> [Entity Attempt] -> Summary
tally usrs = foldl' inserter (initSumm usrs)
  where
    inserter :: Summary -> Entity Attempt -> Summary
    inserter srs (Entity _ att) =
      case attemptUserId att of -- does the attempt have a user id?
        Nothing  -> srs
        Just uid -> M.adjust (addAttempt (attemptExerciseType att) (attemptIsCorrect att)) uid srs

getSeshatR :: Handler Html
getSeshatR = do
  (usrs :: [Entity User]) <- runDB $ selectList [] []
  (atts :: [Entity Attempt]) <- runDB $ selectList [] []
  let summ = tally usrs atts
      exts = activeExerciseTypes
  defaultLayout $ do
    setTitle "Seshat"
    $(widgetFile "summary")
