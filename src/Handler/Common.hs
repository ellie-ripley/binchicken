{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

import Scoring (Correct(..), Progress(..), updateProgress, zeroProgress)

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")


buildScore
  :: Key User
  -> ExerciseType
  -> Progress
  -> Score
buildScore k et p =
  Score { scoreUserId = k
        , scoreExerciseType = et
        , scoreNumCorrect = totalCorrect p
        , scoreBestStreak = bestStreak p
        , scoreCurrentStreak = currentStreak p
        }

getProg
  :: Score
  -> Progress
getProg s = Progress { totalCorrect = scoreNumCorrect s
                     , bestStreak = scoreBestStreak s
                     , currentStreak = scoreCurrentStreak s
                     }

-- | Update the Score table in light of a correct or incorrect response
updateScore
  :: Key User
  -> ExerciseType
  -> Correct
  -> Handler ()
updateScore k et c =
  do
    currScore <- runDB $ selectFirst [ScoreUserId ==. k, ScoreExerciseType ==. et] []
    case currScore of
      Nothing -> do
        let newScore = buildScore k et (updateProgress c zeroProgress)
        _ <- runDB $ insert newScore
        return ()
      Just (Entity sid s) -> do
        let newScore = buildScore k et (updateProgress c $ getProg s)
        _ <- runDB $ replace sid newScore
        return ()
