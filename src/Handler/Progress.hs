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
      getEntity,
      null,
      pack,
      redirect,
      selectList,
      setTitle,
      Html,
      EntityField(..),
      Yesod(defaultLayout),
      YesodPersist(..),
      widgetFile,
      img_ibis_icon_png,
      YesodAuth(maybeAuthId) )
import GHC.Float.RealFracMethods (roundDoubleInt)
import qualified Data.Map as M
import Database.Esqueleto

import Scoring
  ( Progress(..)
  , Results(..)
  , Summary(..)
  , SummaryRow(..)
  , calculateSummaryRow
  , displayPoints
  , displayPointsEarned
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
        muser <- runDB $ getEntity uid
        case muser of
          Nothing -> error "Logged in as a nonexistent user? This is a bug in the site."
          Just user -> do
            scs <- runDB $ selectList [] []
            let tal = tally [user] scs
                sr  = case M.lookup uid (unSummary tal) of
                          Just succ -> succ
                          Nothing   -> error "Error 2323!"
                tpoin = displayPoints . totalPoints $ calculateSummaryRow sr
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
