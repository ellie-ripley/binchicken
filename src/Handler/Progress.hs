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
      Enum(fromEnum),
      Fractional((/)),
      Monad(return),
      Num((*)),
      Ord(..),
      Show(show),
      Semigroup((<>)),
      Double,
      Int,
      Maybe(Just, Nothing),
      Text,
      error,
      null,
      pack,
      redirect,
      setTitle,
      Html,
      EntityField(AttemptSubmittedAt, AttemptUserId),
      Yesod(defaultLayout),
      YesodPersist(runDB),
      widgetFile,
      img_ibis_icon_png,
      YesodAuth(maybeAuthId) )
import GHC.Float.RealFracMethods (roundDoubleInt)
import qualified Data.Map as M
import qualified Database.Esqueleto.Legacy as E
import Database.Esqueleto.Legacy ((^.), (==.))

import Scoring
  ( Progress(..)
  , Results(..)
  , Summary(..)
  , SummaryRow(..)
  , calculateSummaryRow
  , pointsEarned
  , tally
  , totalPoints
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
        let tal = tally [uid] atts
            sr  = case M.lookup uid (unSummary tal) of
                      Just succ -> succ
                      Nothing   -> error "Error 2323!"
        defaultLayout $ do
            setTitle "Progress"
            $(widgetFile "progress")



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
