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
                           , ($)
                           , runDB
                           , selectList
                           , setTitle
                           , widgetFile
                           )
import Database.Esqueleto.Legacy (BackendKey(..))

import qualified Data.Map as M

import Settings.Binchicken (activeExerciseTypes)
import Scoring
  ( calculateSummary
  , exScore
  , tally
  , totalPoints, calculateSummary
  )




displayUserId :: Key User -> String
displayUserId = show . unSqlBackendKey . unUserKey







getSeshatR :: Handler Html
getSeshatR = do
  (usrs :: [Entity User]) <- runDB $ selectList [] []
  (atts :: [Entity Attempt]) <- runDB $ selectList [] []
  let summ = calculateSummary $ tally usrs atts
      exts = activeExerciseTypes
  defaultLayout $ do
    setTitle "Seshat"
    $(widgetFile "seshat")
