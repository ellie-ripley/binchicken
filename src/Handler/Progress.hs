{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Progress where

import ExerciseType
    ( ExerciseType(..),
      ExerciseTargets(..),
      prettyExerciseName
    )
import Settings.Binchicken (targets, fullStreak, exerciseRoute, activeExerciseTypes)
import Foundation
    ( Route(..),
      Handler )
import Import.NoFoundation
    ( otherwise,
      ($),
      (.),
      fromIntegral,
      Bounded(maxBound, minBound),
      Enum(fromEnum),
      Fractional((/)),
      Monad(return),
      Num((+), (*)),
      Ord((<=), (>=)),
      Show(show),
      Semigroup((<>)),
      Bool(..),
      Double,
      Int,
      Maybe(Just, Nothing),
      repeat,
      Text,
      Map,
      map,
      foldr,
      null,
      pack,
      redirect,
      setTitle,
      Html,
      zip,
      Entity,
      EntityField(AttemptSubmittedAt, AttemptUserId),
      EntityField(AttemptSubmittedAt, AttemptUserId),
      Yesod(defaultLayout),
      YesodPersist(runDB),
      widgetFile,
      Attempt(attemptIsCorrect, attemptExerciseType),
      img_ibis_icon_png,
      YesodAuth(maybeAuthId) )
import GHC.Float.RealFracMethods (roundDoubleInt)
import Data.Map (adjust, fromList, lookup, foldlWithKey')
import qualified Database.Esqueleto.Legacy as E
import Database.Esqueleto.Legacy ((^.), (==.))

import Scoring
  ( PointsEarned(..)
  , Progress(..)
  , pointsEarned
  )

getProgressR :: Handler Html
getProgressR = do
    let exTypes = activeExerciseTypes -- Show only active exercises
    muid <- maybeAuthId
    case muid of
      Nothing -> redirect HomeR
      Just uid -> do
        atts <- runDB $ E.select $
            E.from $ \att -> do
            E.where_ (att ^. AttemptUserId ==. (E.just $ E.val uid))
            E.orderBy [E.desc (att ^. AttemptSubmittedAt)]    --Newest to oldest, so current streak is at head
            return att
        let pm = progressMap atts
            tp = totalPoints pm
        defaultLayout $ do
            setTitle "Progress"
            $(widgetFile "progress")

displayPointsEarned :: PointsEarned -> Text
displayPointsEarned = pack . show . fromEnum

exerciseProgressCurrentId :: ExerciseType -> Text
exerciseProgressCurrentId et = (pack $ show et) <> "-progress-current"

exerciseProgressBestId :: ExerciseType -> Text
exerciseProgressBestId et = (pack $ show et) <> "-progress-best"

streakBarPercent :: ExerciseType -> Int -> Int
streakBarPercent et bestStr
  | bestStr <= 0      = 0
  | bestStr >= maxStr = 100
  | otherwise         = roundDoubleInt $ (fromIntegral $ bestStr * 100) / (fromIntegral maxStr :: Double)
  where maxStr = fullStreak et

progressMap :: [Entity Attempt] -> Map ExerciseType Progress
progressMap = (foldr accum zeroProgressMap) . (map E.entityVal)
  where
    zeroProgress :: Progress
    zeroProgress = Progress 0 0 0

    zeroProgressMap :: Map ExerciseType Progress
    zeroProgressMap = fromList $ zip [minBound..maxBound] (repeat zeroProgress)

    adj :: Bool -> Progress -> Progress
    adj True (Progress cur bes tot) =
      Progress (cur + 1) (if bes <= cur
                          then cur + 1
                          else bes) (tot + 1)
    adj False (Progress _ bes tot) = Progress 0 bes tot

    accum :: Attempt -> Map ExerciseType Progress -> Map ExerciseType Progress
    accum a m = adjust (adj $ attemptIsCorrect a) (attemptExerciseType a) m

totalPoints :: Map ExerciseType Progress -> Int
totalPoints = foldlWithKey' accum 0
  where
    accum :: Int -> ExerciseType -> Progress -> Int
    accum a et p = a + (fromEnum $ pointsEarned (targets et) p)
