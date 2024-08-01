{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.CounterexNonclassical where

import ExerciseType (ExerciseType(..))
import Foundation (Handler, Route (CounterexNonclassicalR))
import Import.NoFoundation
    ( ($),
      (<$>),
      Maybe(..),
      Value,
      Text,
      MonadIO(liftIO),
      get,
      id,
      insertEntity,
      getCurrentTime,
      parseCheckJsonBody,
      returnJson,
      Html,
      YesodPersist(runDB),
      Attempt(..),
      YesodAuth(maybeAuthId)
    )
import Data.Aeson (Result(..), Value(..), (.:))
import Data.Aeson.Types (parseMaybe)

import Scoring (boolToCorrect)
import Settings.Binchicken (RandomArgumentSettings(..), defaultRandomArgumentSettings)
import Logic.Matrices (matrixK3, matrixLP, matrixFDE)

import Handler.Common (updateScore)
import Handler.Counterexamples (getCounterexample, processExercise, prepareResponse)

setts :: RandomArgumentSettings
setts = defaultRandomArgumentSettings CounterexampleNonclassical

getCounterexNonclassicalR :: Handler Html
getCounterexNonclassicalR =
  getCounterexample
    setts
    [matrixK3, matrixLP, matrixFDE]
    CounterexampleNonclassical
    "Nonclassical counterexamples"
    "Finding nonclassical counterexamples"
    "Note that the matrix can change from one argument to the next, so stay alert!"
    CounterexNonclassicalR

postCounterexNonclassicalR :: Handler Value
postCounterexNonclassicalR = do
    rRequestJSON <- (parseCheckJsonBody :: Handler (Result Value))
    case rRequestJSON of
      Error s -> returnJson s -- Did we get a parseable response?
      Success requestJson -> case requestJson of
        Object hm -> -- is the response an Object?
          case parseMaybe id (hm .: "icexExerciseId") of
            Nothing -> returnJson ("No exercise id!" :: Text)
            Just exid -> do
              ex <- runDB $ get exid
              case ex of
                Nothing -> returnJson ("Exercise not in database!" :: Text)
                Just e ->
                  case prepareResponse exid <$> processExercise hm e of
                    Error tx -> returnJson tx
                    Success (responseObj, attempt) -> do
                      maybeCurrentUserId <- maybeAuthId
                      case maybeCurrentUserId of
                          Just uid -> do
                              now <- liftIO getCurrentTime
                              let attempt' = attempt { attemptUserId = Just uid, attemptSubmittedAt = Just now }
                                  corr = attemptIsCorrect attempt
                              insertedAttempt <- runDB $ insertEntity attempt'
                              updateScore uid CounterexampleNonclassical (boolToCorrect corr)
                              returnJson (insertedAttempt, responseObj)
                          Nothing -> returnJson (attempt, responseObj)
        _ -> returnJson ("Something went wrong!" :: Text) -- the response was JSON but not an Object

