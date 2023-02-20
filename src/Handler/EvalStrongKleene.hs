{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.EvalStrongKleene where

import ExerciseType (ExerciseType(..))
import Foundation ( Route(..), Handler )
import Import.NoFoundation
    ( fst,
      snd,
      ($),
      Eq((==)),
      Show(show),
      Generic,
      Maybe(..),
      FromJSON,
      Value,
      ToJSON(toJSON),
      Text,
      MonadIO(liftIO),
      (++),
      map,
      pack,
      insertEntity,
      getCurrentTime,
      parseCheckJsonBody,
      returnJson,
      setTitle,
      (.),
      Html,
      Yesod(defaultLayout),
      YesodPersist(runDB),
      widgetFile,
      Attempt(..),
      YesodAuth(maybeAuthId),
      Bounded(..) )
import Text.Julius (RawJS (..))
import Data.Aeson (Result(..), (.=), object)
import qualified Data.Map.Strict as Map

import Handler.LoginCheck (loginNotifyW)
import Logic.Formulas (Formula, displayAtomic, displayFormula, atomsIn)
import Logic.Random (randomFormulaIO, randomValuationSelectAtomicsIO)
import Settings.Binchicken (RandomFormulaSettings(..), defaultRandomFormulaSettings)
import Logic.Valuations (StrongKleeneStructure(..), Valuation, ValDisplay(..), eval)

data IESKAttempt =
  IESKAttempt { iebFormula :: Formula
             , iebValuation :: Valuation StrongKleeneStructure
             , iebResponse :: StrongKleeneStructure
             } deriving (Generic)

instance ToJSON IESKAttempt
instance FromJSON IESKAttempt

setts :: RandomFormulaSettings
setts = defaultRandomFormulaSettings EvaluateStrongKleene

getEvalStrongKleeneR :: Handler Html
getEvalStrongKleeneR = do
    let buttonList = buttons
        displayFormulaId = "js-display-formula" :: Text
        displayValuationId = "js-display-valuation" :: Text
        displayResultId = "js-display-result" :: Text
        buttonsID = "js-response-buttons" :: Text
    (formula :: Formula) <- liftIO $ randomFormulaIO setts
    let ats = atomsIn formula
    (skval :: Valuation StrongKleeneStructure) <- liftIO $ randomValuationSelectAtomicsIO ats
    defaultLayout $ do
        setTitle "Evaluate in the strong Kleene structure"
        $(widgetFile "eval-strong-kleene")

postEvalStrongKleeneR :: Handler Value
postEvalStrongKleeneR = do
    tryIMC <- (parseCheckJsonBody :: Handler (Result IESKAttempt))
    case tryIMC of
      Error err -> returnJson err
      Success iebAttempt -> do
        let fmla = iebFormula iebAttempt
            vl   = iebValuation iebAttempt
            rsp  = iebResponse iebAttempt
            corr = Just rsp == eval vl fmla
            attempt = Attempt { attemptUserId = Nothing
                              , attemptExerciseType = EvaluateStrongKleene
                              , attemptIsCorrect = corr
                              , attemptExerciseContent = Just . pack $ (show fmla ++ " | " ++ show vl)
                              , attemptSubmittedResponse = Just . pack $ show rsp
                              , attemptSubmittedAt = Nothing
                              }
            responseObj = object [ "rformula" .= (displayFormula $ iebFormula iebAttempt)
                                 , "rval" .= (displayVal $ iebResponse iebAttempt)
                                 ]
        maybeCurrentUserId <- maybeAuthId
        case maybeCurrentUserId of
            Just uid -> do
                now <- liftIO getCurrentTime
                let attempt' = attempt { attemptUserId = Just uid, attemptSubmittedAt = Just now }
                insertedAttempt <- runDB $ insertEntity attempt'
                returnJson (insertedAttempt, responseObj)
            Nothing -> returnJson (attempt, responseObj)

buttons :: [(Text, StrongKleeneStructure)]
buttons = map (\b -> ("js-button-" ++ pack (show b), b)) [minBound..maxBound]
