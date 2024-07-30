{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Seshat where

import Foundation ( Handler )
import Import.NoFoundation ( Attempt(..)
                           , Entity(..)
                           , EntityField(..)
                           , Exercise(..)
                           , Html
                           , Key(..)
                           , Score(..)
                           , User(..)
                           , Yesod(..)
                           , runDB
                           , selectList
                           , setTitle
                           , widgetFile
                           )
import Database.Esqueleto

import qualified Data.Map as M

import Settings.Binchicken (activeExerciseTypes)
import Scoring
  ( calculateSummary
  , exScore
  , tally
  , totalPoints
  , Summary(..)
  )


displayUserId :: Key User -> String
displayUserId = show . unSqlBackendKey . unUserKey

displayScore :: Maybe Int -> String
displayScore =
  \case Nothing -> "Missing"
        Just i  -> show i

getSeshatR :: Handler Html
getSeshatR = do
  (usrs :: [Entity User]) <- runDB $ selectList [] []
  (scs :: [Entity Score]) <- runDB $ selectList [] []
  let summ = calculateSummary $ tally usrs scs
      exts = activeExerciseTypes
  defaultLayout $ do
    setTitle "Seshat"
    $(widgetFile "seshat")
