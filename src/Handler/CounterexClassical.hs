{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.CounterexClassical where

import ExerciseType (ExerciseType(..))
import Foundation (Route(..), Handler, Widget)
import Import.NoFoundation
    ( ($),
      (<>),
      (<$>),
      Generic,
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
      YesodAuth(maybeAuthId),
      Bounded(..),
      hamlet,
      julius,
      lucius,
      whamlet,
      toWidget,
      newIdent)
import Text.Julius (RawJS (..))
import Data.Aeson (FromJSON, ToJSON, Result(..), Value(..), (.:))
import Data.Aeson.Types (parseMaybe)

import Logic.Formulas (Atomic(..), displayAtomic)
import Scoring (boolToCorrect)
import Settings.Binchicken (RandomArgumentSettings(..), defaultRandomArgumentSettings)
import Logic.Valuations (BooleanStructure(..), Valuation, ValDisplay(..), displayVal)
import Logic.Arguments (Argument(..))
import Logic.Matrices (matrixCL)
import Handler.Counterexamples (getCounterexample, prepareResponse, processExercise)
import Handler.Common (updateScore)

data ICCAttempt =
  ICCAttempt { iccArgument :: Argument
             , iccValuation :: Maybe (Valuation BooleanStructure) } deriving (Generic)

instance ToJSON ICCAttempt
instance FromJSON ICCAttempt

setts :: RandomArgumentSettings
setts = defaultRandomArgumentSettings CounterexampleClassical

valsToChooseFrom :: [BooleanStructure]
valsToChooseFrom = [minBound..maxBound]

getCounterexClassicalR :: Handler Html
getCounterexClassicalR =
  getCounterexample
    setts
    [matrixCL]
    CounterexampleClassical
    "Classical counterexamples"
    "Finding classical counterexamples"
    ""
    CounterexClassicalR

postCounterexClassicalR :: Handler Value
postCounterexClassicalR = do
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
                                updateScore uid CounterexampleClassical (boolToCorrect corr)
                                returnJson (insertedAttempt, responseObj)
                            Nothing -> returnJson (attempt, responseObj)
        _ -> returnJson ("Something went wrong!" :: Text) -- the response was JSON but not an Object

-- | An id for an atom, used to display current value
-- | Prefix split out for use in Julius
atomDisplayValIdPrefix :: Text
atomDisplayValIdPrefix = "js-value-for-"

atomDisplayValId :: Atomic -> Text
atomDisplayValId (At t) = atomDisplayValIdPrefix <> t


-- | A single button for setting the value of an atomic sentence
atValWidget
  :: ValDisplay v
  => v      -- ^ The value this button should set
  -> Atomic -- ^ The atomic the value is set for
  -> Widget
atValWidget vlu at = do
  let vdid = atomDisplayValId at
  valButtonId <- newIdent
  toWidget [hamlet|
              <td .oblang>
                <button .btn.btn-primary ##{valButtonId}>#{displayVal vlu}
           |]
  toWidget [julius|
                $("#{rawJS $ "#" <> valButtonId}").click(function() {
                    document.getElementById("#{rawJS vdid}").innerHTML = #{displayVal vlu};
                });
           |]
    
valWidget :: ValDisplay v => [v] -> Atomic -> Widget
valWidget vlus at = do
  let vdid = atomDisplayValId at
  toWidget [whamlet|
            <tr>
              <td .oblang.atomic>#{displayAtomic at}
              $forall vlu <- vlus
                ^{atValWidget vlu at}
              <td>
                <p .atomic-value ##{vdid}>
           |]
  toWidget [lucius|
              .atom { padding: 0px 20px; }
           |]
