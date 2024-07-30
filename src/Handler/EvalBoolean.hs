{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.EvalBoolean where

import ExerciseType (ExerciseType(..))
import Foundation ( Route(..), Handler )
import Import.NoFoundation
    ( fst,
      snd,
      ($),
      Entity(..),
      Eq((==)),
      Exercise(..),
      Key,
      Show(show),
      Generic,
      Maybe(..),
      FromJSON,
      Value,
      ToJSON(toJSON),
      Text,
      MonadIO(liftIO),
      (++),
      error,
      fromMaybe,
      getEntity,
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
import Data.Aeson (Result(..), (.=), object, decodeStrict)
import qualified Data.Map.Strict as Map
import Data.Text.Encoding (encodeUtf8)

import Handler.LoginCheck (loginNotifyW)
import Logic.Formulas
  ( Formula,
    GenFormula(..),
    NullaryConnective(..),
    displayAtomic,
    displayFormula,
    atomsIn )
import Logic.Random (randomFormulaIO, randomValuationSelectAtomicsIO)
import Settings.Binchicken (RandomFormulaSettings(..), defaultRandomFormulaSettings)
import Logic.Valuations (BooleanStructure(..), Valuation, ValDisplay(..), eval)

data IEBAttempt =
  IEBAttempt { iebExerciseId :: Key Exercise
             , iebResponse :: BooleanStructure
             } deriving (Generic)

instance ToJSON IEBAttempt
instance FromJSON IEBAttempt

setts :: RandomFormulaSettings
setts = defaultRandomFormulaSettings EvaluateBoolean

getEvalBooleanR :: Handler Html
getEvalBooleanR = do
    let buttonList = buttons
        displayFormulaId = "js-display-formula" :: Text
        displayValuationId = "js-display-valuation" :: Text
        displayResultId = "js-display-result" :: Text
        buttonsID = "js-response-buttons" :: Text
    (formula :: Formula) <- liftIO $ randomFormulaIO setts
    let ats = atomsIn formula
    (bval :: Valuation BooleanStructure) <- liftIO $ randomValuationSelectAtomicsIO ats
    defaultLayout $ do
        setTitle "Evaluate in the Boolean structure"
        $(widgetFile "eval-boolean")

decodeEB :: Text -> Maybe (Formula, Valuation BooleanStructure)
decodeEB bs = decodeStrict $ encodeUtf8 bs

postEvalBooleanR :: Handler Value
postEvalBooleanR = do
    tryIMC <- (parseCheckJsonBody :: Handler (Result IEBAttempt))
    case tryIMC of
      Error err -> returnJson err
      Success iebAttempt -> do
        mex <- runDB $ getEntity (iebExerciseId iebAttempt)
        case mex of
          Nothing -> error "No such exercise has been generated!"
          Just (Entity exid ex) -> do
            let rsp  = iebResponse iebAttempt
                (fmla, vl) = fromMaybe (N Verum, Map.empty) (decodeEB $ exerciseExerciseContent ex)
                corr = Just rsp == eval vl fmla
                attempt = Attempt { attemptUserId = Nothing
                                  , attemptExerciseId = exid
                                  , attemptIsCorrect = corr
                                  , attemptSubmittedResponse = Just . pack $ show rsp
                                  , attemptSubmittedAt = Nothing
                                  }
                responseObj = object [ "rexercise" .= (show $ iebExerciseId iebAttempt)
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

buttons :: [(Text, BooleanStructure)]
buttons = map (\b -> ("js-button-" ++ pack (show b), b)) [minBound..maxBound]
