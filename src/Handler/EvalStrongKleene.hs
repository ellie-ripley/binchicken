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
      Entity(..),
      Eq((==)),
      Exercise(..),
      Show(show),
      Generic,
      Key,
      Maybe(..),
      FromJSON,
      SentExercise(..),
      Value,
      ToJSON(toJSON),
      Text,
      MonadIO(liftIO),
      (++),
      error,
      fromMaybe,
      getEntity,
      insert,
      map,
      pack,
      insertEntity,
      getCurrentTime,
      parseCheckJsonBody,
      returnJson,
      setTitle,
      toStrict,
      (.),
      Html,
      Yesod(defaultLayout),
      YesodPersist(runDB),
      widgetFile,
      Attempt(..),
      YesodAuth(maybeAuthId),
      Bounded(..) )
import Text.Julius (RawJS (..))
import Data.Aeson (Result(..), (.=), object, decodeStrict, encode)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Map.Strict as Map

import Handler.Common (updateScore)
import Handler.LoginCheck (loginNotifyW)
import Logic.Formulas
  ( Formula,
    GenFormula(..),
    NullaryConnective(..),
    displayAtomic,
    displayFormula,
    atomsIn )
import Logic.Random (randomFormulaIO, randomValuationSelectAtomicsIO)
import Scoring (boolToCorrect)
import Settings.Binchicken (RandomFormulaSettings(..), defaultRandomFormulaSettings)
import Logic.Valuations (StrongKleeneStructure(..), Valuation, ValDisplay(..), eval)

data IESKAttempt =
  IESKAttempt { ieskExerciseId :: Key Exercise
              , ieskResponse :: StrongKleeneStructure
              } deriving (Generic)

instance ToJSON IESKAttempt
instance FromJSON IESKAttempt

setts :: RandomFormulaSettings
setts = defaultRandomFormulaSettings EvaluateStrongKleene


decodeESK :: Text -> Maybe (Formula, Valuation StrongKleeneStructure)
decodeESK bs = decodeStrict $ encodeUtf8 bs

encodeESK
  :: Formula
  -> Valuation StrongKleeneStructure
  -> Text
encodeESK fm vsk = decodeUtf8 . toStrict $ encode (fm, vsk)

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
    let ex = Exercise { exerciseExerciseType = EvaluateStrongKleene
                      , exerciseExerciseContent = encodeESK formula skval
                      }
    exid <- runDB $ insert ex
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
      Just uid -> do
        now <- liftIO getCurrentTime
        let sent = SentExercise { sentExerciseUserId = Just uid
                                , sentExerciseExerciseId = exid
                                , sentExerciseSentAt = Just now
                                }
        _ <- runDB $ insert sent
        defaultLayout $ do
            setTitle "Evaluate in the Strong Kleene structure"
            $(widgetFile "eval-strong-kleene")
      Nothing -> defaultLayout $ do
        setTitle "Evaluate in the strong Kleene structure"
        $(widgetFile "eval-strong-kleene")



postEvalStrongKleeneR :: Handler Value
postEvalStrongKleeneR = do
    tryIMC <- (parseCheckJsonBody :: Handler (Result IESKAttempt))
    case tryIMC of
      Error err -> returnJson err
      Success ieskAttempt -> do
        mex <- runDB $ getEntity (ieskExerciseId ieskAttempt)
        case mex of
          Nothing -> error "No such exercise has been given!"
          Just (Entity exid ex) -> do
            let rsp = ieskResponse ieskAttempt
                (fmla, vl) = fromMaybe (N Verum, Map.empty) (decodeESK $ exerciseExerciseContent ex)
                corr = Just rsp == eval vl fmla
                attempt = Attempt { attemptUserId = Nothing
                                  , attemptExerciseId = exid
                                  , attemptIsCorrect = corr
                                  , attemptSubmittedResponse = Just . pack $ show rsp
                                  , attemptSubmittedAt = Nothing
                                  }
                responseObj = object [ "rformula" .= (displayFormula fmla)
                                     , "rval" .= (displayVal $ ieskResponse ieskAttempt)
                                     ]
            maybeCurrentUserId <- maybeAuthId
            case maybeCurrentUserId of
                Just uid -> do
                    now <- liftIO getCurrentTime
                    let attempt' = attempt { attemptUserId = Just uid, attemptSubmittedAt = Just now }
                    insertedAttempt <- runDB $ insertEntity attempt'
                    updateScore uid EvaluateStrongKleene (boolToCorrect corr)
                    returnJson (insertedAttempt, responseObj)
                Nothing -> returnJson (attempt, responseObj)

buttons :: [(Text, StrongKleeneStructure)]
buttons = map (\b -> ("js-button-" ++ pack (show b), b)) [minBound..maxBound]
