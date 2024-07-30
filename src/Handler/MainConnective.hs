{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.MainConnective where

import ExerciseType ( ExerciseType(IdentifyMainConnective) )
import Foundation ( Route(MainConnectiveR), Handler )
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
      Value,
      ToJSON(toJSON),
      Text,
      MonadIO(liftIO),
      (++),
      error,
      fromMaybe,
      map,
      pack,
      getEntity,
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
      YesodAuth(maybeAuthId) )
import Text.Julius (RawJS (..))
import Data.Aeson (Result(..), (.=), decodeStrict, object)
import Data.Text.Encoding (encodeUtf8)

import Handler.LoginCheck (loginNotifyW)
import Logic.Formulas
  ( Connective(..),
    NullaryConnective(..),
    UnaryConnective(..),
    BinaryConnective(..),
    Formula,
    GenFormula(..),
    displayConnective,
    displayFormula,
    displayMaybeConnective,
    mainConnective,
    nameConnective )
import Logic.Random (randomFormulaIO)
import Settings.Binchicken (RandomFormulaSettings(..), defaultRandomFormulaSettings)

data IMCAttempt =
  IMCAttempt { imcExerciseId :: Key Exercise
             , imcResponse :: Maybe Connective
             } deriving (Generic)

instance ToJSON IMCAttempt
instance FromJSON IMCAttempt

setts :: RandomFormulaSettings
setts = defaultRandomFormulaSettings IdentifyMainConnective

mcConns :: [Connective]
mcConns = map CN (rfNullaryConns setts :: [NullaryConnective]) -- type signature suppresses erroneous warning
            ++ map CU (rfUnaryConns setts)
            ++ map CB (rfBinaryConns setts)

getMainConnectiveR :: Handler Html
getMainConnectiveR = do
    let buttonList = buttons
        displayFormulaId = "js-display-formula" :: Text
        displayResultId = "js-display-result" :: Text
        noMcButtonLabel = "js-button-no-main-connective" :: Text
        buttonsId = "js-response-buttons" :: Text
        nuttin = Nothing :: Maybe Connective
    (formula :: Formula) <- liftIO $ randomFormulaIO setts
    defaultLayout $ do
        setTitle "Identify the main connective"
        $(widgetFile "main-connective")

decodeMC :: Text -> Maybe Formula
decodeMC tx = decodeStrict (encodeUtf8 tx)

postMainConnectiveR :: Handler Value
postMainConnectiveR = do
    tryIMC <- (parseCheckJsonBody :: Handler (Result IMCAttempt))
    case tryIMC of
      Error err -> returnJson err
      Success imcAttempt -> do
        mex <- runDB $ getEntity (imcExerciseId imcAttempt)
        case mex of
          Nothing -> error "No such exercise has been given!"
          Just (Entity exid ex) -> do
            let rsp = imcResponse imcAttempt
                fmla = fromMaybe (N Verum) (decodeMC $ exerciseExerciseContent ex)
                corr = rsp == mainConnective fmla
                attempt = Attempt { attemptUserId = Nothing
                                  , attemptExerciseId = exid
                                  , attemptIsCorrect = corr
                                  , attemptSubmittedResponse = case imcResponse imcAttempt of
                                      Nothing -> Nothing
                                      Just con -> Just (pack $ show con)
                                  , attemptSubmittedAt = Nothing
                                  }
                responseObj = object [ "rexercise" .= (show $ imcExerciseId imcAttempt)
                                    , "rconn" .= (displayMaybeConnective $ imcResponse imcAttempt)
                                    ]
            maybeCurrentUserId <- maybeAuthId
            case maybeCurrentUserId of
                Just uid -> do
                    now <- liftIO getCurrentTime
                    let attempt' = attempt { attemptUserId = Just uid, attemptSubmittedAt = Just now }
                    insertedAttempt <- runDB $ insertEntity attempt'
                    returnJson (insertedAttempt, responseObj)
                Nothing -> returnJson (attempt, responseObj)

buttons :: [(Text, Connective)]
buttons = map (\c -> ("js-button-" ++ nameConnective c, c)) mcConns
