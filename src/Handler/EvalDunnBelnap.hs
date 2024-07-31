{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.EvalDunnBelnap where

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
import qualified Data.Map.Strict as Map
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

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
import Logic.Valuations (DunnBelnapStructure(..), Valuation, ValDisplay(..), eval)

data IEDBAttempt =
  IEDBAttempt { iedbExerciseId :: Key Exercise
              , iedbResponse :: DunnBelnapStructure
              } deriving (Generic)

instance ToJSON IEDBAttempt
instance FromJSON IEDBAttempt

setts :: RandomFormulaSettings
setts = defaultRandomFormulaSettings EvaluateDunnBelnap


decodeEDB :: Text -> Maybe (Formula, Valuation DunnBelnapStructure)
decodeEDB bs = decodeStrict $ encodeUtf8 bs

encodeEDB
  :: Formula
  -> Valuation DunnBelnapStructure
  -> Text
encodeEDB fm vdb = decodeUtf8 . toStrict $ encode (fm, vdb)

getEvalDunnBelnapR :: Handler Html
getEvalDunnBelnapR = do
    let buttonList = buttons
        displayFormulaId = "js-display-formula" :: Text
        displayValuationId = "js-display-valuation" :: Text
        displayResultId = "js-display-result" :: Text
        buttonsID = "js-response-buttons" :: Text
    (formula :: Formula) <- liftIO $ randomFormulaIO setts
    let ats = atomsIn formula
    (dbval :: Valuation DunnBelnapStructure) <- liftIO $ randomValuationSelectAtomicsIO ats
    let ex = Exercise { exerciseExerciseType = EvaluateDunnBelnap
                      , exerciseExerciseContent = encodeEDB formula dbval
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
            setTitle "Evaluate in the Dunn-Belnap structure"
            $(widgetFile "eval-dunn-belnap")
      Nothing -> defaultLayout $ do
        setTitle "Evaluate in the Dunn-Belnap structure"
        $(widgetFile "eval-dunn-belnap")


postEvalDunnBelnapR :: Handler Value
postEvalDunnBelnapR = do
    tryIMC <- (parseCheckJsonBody :: Handler (Result IEDBAttempt))
    case tryIMC of
      Error err -> returnJson err
      Success iedbAttempt -> do
        mex <- runDB $ getEntity (iedbExerciseId iedbAttempt)
        case mex of
          Nothing -> error "No such exercise has been generated!"
          Just (Entity exid ex) -> do
            let rsp  = iedbResponse iedbAttempt
                (fmla, vl) = fromMaybe (N Verum, Map.empty) (decodeEDB $ exerciseExerciseContent ex)
                corr = Just rsp == eval vl fmla
                attempt = Attempt { attemptUserId = Nothing
                                  , attemptExerciseId = exid
                                  , attemptIsCorrect = corr
                                  , attemptSubmittedResponse = Just . pack $ show rsp
                                  , attemptSubmittedAt = Nothing
                                  }
                responseObj = object [ "rformula" .= (displayFormula fmla)
                                     , "rval" .= (displayVal $ iedbResponse iedbAttempt)
                                     ]
            maybeCurrentUserId <- maybeAuthId
            case maybeCurrentUserId of
                Just uid -> do
                    now <- liftIO getCurrentTime
                    let attempt' = attempt { attemptUserId = Just uid, attemptSubmittedAt = Just now }
                    insertedAttempt <- runDB $ insertEntity attempt'
                    updateScore uid EvaluateDunnBelnap (boolToCorrect corr)
                    returnJson (insertedAttempt, responseObj)
                Nothing -> returnJson (attempt, responseObj)

buttons :: [(Text, DunnBelnapStructure)]
buttons = map (\b -> ("js-button-" ++ pack (show b), b)) [minBound..maxBound]
