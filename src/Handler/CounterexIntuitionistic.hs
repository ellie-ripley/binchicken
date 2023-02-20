{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.CounterexIntuitionistic where

import Import.NoFoundation ( Attempt(..)
                           , Html
                           , Value
                           , defaultLayout
                           , getCurrentTime
                           , hamlet
                           , insertEntity
                           , liftIO
                           , parseCheckJsonBody
                           , returnJson
                           , runDB
                           , setTitle
                           , toWidgetHead
                           , widgetFile, YesodAuth (maybeAuthId)
                           )
import Foundation ( Handler
                  , Route(..)
                  )

import Data.Aeson (ToJSON, FromJSON, Result(..), (.=), object, toJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import GHC.Generics (Generic)
import Text.Julius (RawJS(..))
import Text.Megaparsec (parseMaybe)

import ExerciseType (ExerciseType(..))
import Handler.LoginCheck (loginNotifyW)
import Handler.Proofs (singleProofEntry)
import Logic.Arguments (Argument(..))
import Logic.Formulas (displayFormula)
import Logic.Models.Intuitionistic ( ModelFeedback(..)
                                   , displayModel
                                   , modelIsCounterexample
                                   , preParse
                                   , parseModel
                                   )
import Logic.PreProofs ( RawProofTree(..)
                       , Rule(..)
                       , UnaryRule(..)
                       , TrinaryRule(..)
                       , displayPPPError
                       , readPreProof
                       , ruleName
                       )
import Logic.Proofs ( checkProofOf
                    , displayCheckProofOf
                    , CheckProofOf(..)
                    )
import Logic.Random (randomArgumentIO)
import Settings.Binchicken (RandomArgumentSettings(..), defaultRandomArgumentSettings)


data ModelOrProof = Mod | Prf
  deriving (Generic, ToJSON, FromJSON)

data ICIAttempt =
  ICIAttempt
        { iciArgument :: Argument
        , iciModel :: Maybe Text
        , iciProof :: Maybe RawProofTree
        , iciMorP :: ModelOrProof
        } deriving (Generic, ToJSON, FromJSON)




setts :: RandomArgumentSettings
setts = defaultRandomArgumentSettings CounterexampleIntuitionistic

getCounterexIntuitionisticR :: Handler Html
getCounterexIntuitionisticR = do
  arg@(Argument prems conc) <- liftIO $ randomArgumentIO setts
  let ajaxRoute = CounterexIntuitionisticR
      startingConclusion = conc
      (buttonSubmitProofId, buttonSubmitModelId, buttonModDivId, buttonPrfDivId) = buttonIds
      (feedbackId, proofId, modelId) = otherIds
      proofWidget = singleProofEntry proofId ""
  defaultLayout $ do
    setTitle "Decide an aargument"
    toWidgetHead [hamlet|<script src=/static/js/proof.js>|]
    toWidgetHead
          [hamlet|<link rel=stylesheet href=static/css/proof.css type="text/css" media="screen" title="no title" charset="utf-8">|]
    $(widgetFile "counterexample-intuitionistic")


postCounterexIntuitionisticR :: Handler Value
postCounterexIntuitionisticR = do
  tryResponse <- (parseCheckJsonBody :: Handler (Result ICIAttempt))
  case tryResponse of
    Error err -> returnJson err
    Success iciAttempt -> do
      let (responseObj, submitted, corr) =
                case iciMorP iciAttempt of
                  Mod -> case iciModel iciAttempt of
                    Nothing -> ( object [ "feedback" .= ("Bug in site! This should not be possible! Alert Dave, please! Code 23." :: Text) ]
                                        , ""
                                        , False
                                        )
                    Just modTxt -> case parseMaybe parseModel (preParse modTxt) of
                      Nothing -> ( object [ "feedback" .= ("Can't read model!" :: Text) ]
                                          , ""
                                          , False)
                      Just md -> case modelIsCounterexample md (iciArgument iciAttempt) of
                        IsCounterexample m _ ->
                          ( object [ "feedback" .= ("Yes! " <> displayModel m <> " is a counterexample to this argument.") ]
                          , encodeToLazyText ( object [ "modOrProof" .= toJSON Mod
                                                      , "model" .= toJSON md ] )
                          , True)
                        IsNotCounterexample m _ ->
                          ( object [ "feedback" .= ("Nope! " <> displayModel m <> " is not a counterexample to this argument.") ]
                          , encodeToLazyText ( object [ "modOrProof" .= toJSON Mod
                                                      , "model" .= toJSON md] )
                          , False)
                  Prf -> case iciProof iciAttempt of
                    Nothing -> ( object [ "feedback" .= ("No proof submitted? This should not be possible! Alert Dave, please. Code 32." :: Text) ]
                               , encodeToLazyText ( object [ "modOrProof" .= toJSON Prf
                                                           , "proof" .= ("Missing" :: Text) ] )
                               , False)
                    Just rpt -> case readPreProof rpt of
                      Left err -> ( object [ "feedback" .= toJSON (displayPPPError err) ]
                                  , encodeToLazyText ( object [ "modOrProof" .= toJSON Prf
                                                              , "proof" .= toJSON rpt ] )
                                  , False)
                      Right pp ->
                        case checkProofOf (iciArgument iciAttempt) pp of
                          ProofOfArgument arg ->
                            ( object [ "feedback" .= toJSON (displayCheckProofOf (ProofOfArgument arg)) ]
                            , encodeToLazyText ( object [ "modOrProof" .= toJSON Prf
                                                        , "proof" .= toJSON rpt ] )
                            , True)
                          oops ->
                            ( object [ "feedback" .= toJSON (displayCheckProofOf oops) ]
                            , encodeToLazyText ( object [ "modOrProof" .=  toJSON Prf
                                                        , "proof" .= toJSON rpt ])
                            , False)
          attempt = Attempt { attemptUserId = Nothing
                            , attemptExerciseType = CounterexampleIntuitionistic
                            , attemptIsCorrect = corr
                            , attemptExerciseContent = Just (pack . show . toJSON $ iciArgument iciAttempt)
                            , attemptSubmittedResponse = Just (toStrict submitted)
                            , attemptSubmittedAt = Nothing
                            }
      maybeCurrentUserId <- maybeAuthId
      case maybeCurrentUserId of
        Just uid -> do
          now <- liftIO getCurrentTime
          let attempt' = attempt { attemptUserId = Just uid, attemptSubmittedAt = Just now }
          insertedAttempt <- runDB $ insertEntity attempt'
          returnJson (insertedAttempt, responseObj)
        Nothing -> returnJson (attempt, responseObj)


buttonIds :: (Text, Text, Text, Text)
buttonIds = ( "js-submit-proof-id"
            , "js-submit-model-id"
            , "js-button-model-div-id"
            , "js-button-proof-div-id"
            )

otherIds :: (Text, Text, Text)
otherIds = ( "js-feedback-id"
           , "js-proof-id"
           , "js-model-id"
           )
