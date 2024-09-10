{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ProveArgument where

import ExerciseType (ExerciseType(..))
import Foundation
  ( Route(..), Handler )
import Import.NoFoundation
  ( Attempt(..)
  , Bool(..)
  , Either(..)
  , Entity(..)
  , Exercise(..)
  , FromJSON(..)
  , Html
  , Int
  , IO
  , Key
  , Maybe(..)
  , SentExercise(..)
  , Text
  , Value
  , ($)
  , (+)
  , (.)
  , (.=)
  , (.:)
  , (<$>)
  , (<*>)
  , (<>)
  , (==)
  , (>)
  , defaultLayout
  , error
  , fromMaybe
  , getCurrentTime
  , getEntity
  , hamlet
  , insert
  , insertEntity
  , liftIO
  , maybeAuthId
  , null
  , object
  , pack
  , parseCheckJsonBody
  , returnJson
  , runDB
  , setTitle
  , show
  , splitAt
  , toJSON
  , toStrict
  , toWidgetHead
  , whamlet
  , widgetFile
  )

import Data.Aeson (Result(..), decodeStrict, encode, withObject)
import Data.List (nub, (\\))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified System.Random as SR
import Text.Julius (rawJS)

import Handler.Common (updateScore)
import Handler.LoginCheck (loginNotifyW)
import Handler.Proofs (singleProofEntry)
import Logic.Arguments (Argument(..))
import Logic.Formulas (atom, displayFormula)
import Logic.Random (randomFormula, randomFormulas, randomIntValidArgumentIO)
import Logic.PreProofs ( RawProofTree(..)
                       , Rule(..)
                       , UnaryRule(..)
                       , BinaryRule(..)
                       , TrinaryRule(..)
                       , randomRuleList
                       , readPreProof
                       , ruleName
                       , displayPPPError
                       )
import Logic.Proofs
import Scoring (boolToCorrect)
import Settings.Binchicken (RandomFormulaSettings(..), defaultRandomFormulaSettings)



data PRAttempt =
  PRAttempt { prExerciseId :: Key Exercise
            , prRawProof :: RawProofTree
            }

instance FromJSON PRAttempt where
  parseJSON = withObject "PRAttempt" $ \v -> PRAttempt
    <$> v .: "prExerciseId"
    <*> v .: "prRawProof"

fmSetts :: RandomFormulaSettings
fmSetts = defaultRandomFormulaSettings ProveAnArgument

-- | want decodePV (encodePV ta) == ta, for all Arguments ta
decodePV :: Text -> Maybe Argument
decodePV tx = decodeStrict $ encodeUtf8 tx

encodePV :: Argument -> Text
encodePV pr = decodeUtf8 . toStrict $ encode pr

getProveArgumentR :: Handler Html
getProveArgumentR = do
  targetArg@(Argument prems conc) <- liftIO $ randomIntValidArgumentIO fmSetts
  let ex = Exercise { exerciseExerciseType = ProveAnArgument
                    , exerciseExerciseContent = encodePV targetArg }
      (buttonSubmitId, buttonDivId) = buttonIds
      (negButt, conjButt, disjButt, implButt, fumButt) = connButtonIds
      (proofIdPrefix, feedbackId, displayArgumentId) = divIds
      proofId = proofIdPrefix <> "1"
      startingConclusion = atom "p"
      pExName = "Prove an argument" :: Text
      pExInstructions =
        [whamlet|<p>Here is an intuitionistically valid argument. Give a proof of it in NJ. (You might want to work this out on scratch paper first!)|]
      pExEntry = singleProofEntry proofId ""
      ajaxRoute = ProveArgumentR
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
        setTitle "Proofs in NJ"
        toWidgetHead [hamlet|<script src=/static/js/proof.js>|]
        toWidgetHead
            [hamlet|<link rel=stylesheet href=static/css/proof.css type="text/css" media="screen" title="no title" charset="utf-8">|]
        $(widgetFile "prove-int-layout")
    Nothing -> defaultLayout $ do
      setTitle "Proofs in NJ"
      toWidgetHead [hamlet|<script src=/static/js/proof.js>|]
      toWidgetHead
          [hamlet|<link rel=stylesheet href=static/css/proof.css type="text/css" media="screen" title="no title" charset="utf-8">|]
      $(widgetFile "prove-int-layout")

-- TODO: This is a hack, setting up a fake ridiculous argument that is not generatable,
-- for use when we can't read an argument out of the database-stored exercise, which shouldn't happen
obviouslyInvalidArg :: Argument
obviouslyInvalidArg = Argument [atom "!*%^&%"] (atom "!(&^#*$^%)")

postProveArgumentR :: Handler Value
postProveArgumentR = do
  tryResponse <- (parseCheckJsonBody :: Handler (Result PRAttempt))
  case tryResponse of
    Error err -> returnJson err
    Success prAttempt -> do
      mex <- runDB $ getEntity (prExerciseId prAttempt)
      case mex of
        Nothing -> error "No such exercise has been given!"
        Just (Entity exid ex) -> do
          let targ = fromMaybe obviouslyInvalidArg (decodePV $ exerciseExerciseContent ex)
              (responseObj, corr) = case (readPreProof . prRawProof $ prAttempt) of
                                        Left err -> (object [ "feedback" .= toJSON (displayPPPError err) ], False)
                                        Right pp ->
                                            case checkProofOf targ pp of
                                              good@(ProofOfArgument _) ->
                                                (object [ "feedback" .= toJSON (displayCheckProofOf good)], True)
                                              oops ->
                                                (object [ "feedback" .= toJSON (displayCheckProofOf oops)], False)
              attempt = Attempt { attemptUserId = Nothing
                                 , attemptExerciseId = exid
                                 , attemptIsCorrect = corr
                                 , attemptSubmittedResponse = Just (pack .  show . toJSON $ prRawProof prAttempt)
                                 , attemptSubmittedAt = Nothing
                                 }
          maybeCurrentUserId <- maybeAuthId
          case maybeCurrentUserId of
              Just uid -> do
                  now <- liftIO getCurrentTime
                  let attempt' = attempt { attemptUserId = Just uid, attemptSubmittedAt = Just now }
                  insertedAttempt <- runDB $ insertEntity attempt'
                  updateScore uid ProveAnArgument (boolToCorrect corr)
                  returnJson (insertedAttempt, responseObj)
              Nothing -> returnJson (attempt, responseObj)

buttonIds :: (Text, Text)
buttonIds = ("js-button-submit-proof", "js-button-div")

connButtonIds :: (Text, Text, Text, Text, Text)
connButtonIds = ("js-neg-button", "js-conj-button", "js-disj-button", "js-impl-button", "js-fum-button")

divIds :: (Text, Text, Text)
divIds = ("js-proof", "js-feedback", "js-display-argument")
