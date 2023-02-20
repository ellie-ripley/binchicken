{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ProofIntuitionistic where

import Import

import Data.Aeson (Result(..), withObject)
import Text.Julius (rawJS)

import Logic.Sequents ( randomIntValidArgFilterIO
                      )
import Logic.PreProofs
    ( Rule(RT, RU),
      TrinaryRule(DE),
      UnaryRule(II, NI),
      RawProofTree,
      ruleName,
      displayPPPError,
      readPreProof )


import Handler.Proofs (singleProofEntry)
import Settings.Binchicken ( defRandomSequentPreProofSettings
                           , setMaxComplexity
                           , setMaxPremises
                           , RandomSequentPreProofSettings
                           )
import Handler.LoginCheck (loginNotifyW)
import Logic.Arguments (Argument(..))
import Logic.Formulas (displayFormula)
import Logic.Proofs ( checkProofOf
                    , displayCheckProofOf
                    , CheckProofOf(..)
                    )

data PIAttempt =
  PIAttempt { piArgument :: Argument
            , piProof :: RawProofTree
            }

instance FromJSON PIAttempt where
  parseJSON = withObject "PIAttempt" $ \v -> PIAttempt
    <$> v .: "pExContentPost"
    <*> v .: "pExSubmittedProof"

setts :: RandomSequentPreProofSettings
setts = setMaxPremises 2 . (setMaxComplexity 2) $ defRandomSequentPreProofSettings

isId :: Argument -> Bool
isId (Argument prems concl) = concl `elem` prems

getProofIntuitionisticR :: Handler Html
getProofIntuitionisticR = do
  arg@(Argument prems concl) <- liftIO $ randomIntValidArgFilterIO (not. isId) setts
  let (buttonDivId, buttonSubmitId) = buttonIds
      (proofIdPrefix, feedbackId, displayArgumentId) = otherIds
      proofId = proofIdPrefix <> "1"
      startingConclusion = concl
      pExName = "Proof of an argument" :: Text
      pExInstructions =
        [whamlet|<p>Here is an intuitionistically valid argument. (Premises above the line; conclusion below it.)
                 <p>Give a proof of this argument in NJ.
        |]
      pExContent =
        [whamlet|<div .oblang ##{displayArgumentId}>
                   <ul .argument>
                     $forall prem <- prems
                        <li>#{displayFormula prem}
                     <li>---------------
                     <li>#{displayFormula concl}
        |]
      pExEntry = singleProofEntry proofId ""
      ajaxRoute = ProofIntuitionisticR
      pExContentPost = toJSON arg
  defaultLayout $ do
    setTitle "Proofs in NJ"
    toWidgetHead [hamlet|<script src=/static/js/proof.js>|]
    toWidgetHead
        [hamlet|<link rel=stylesheet href=static/css/proof.css type="text/css" media="screen" charset="utf-8">|]
    $(widgetFile "proofs-layout")

postProofIntuitionisticR :: Handler Value
postProofIntuitionisticR = do
  tryResponse <- (parseCheckJsonBody :: Handler (Result PIAttempt))
  case tryResponse of
    Error err -> returnJson err
    Success piAttempt -> do
      let tryPreProof = readPreProof . piProof $ piAttempt
          (responseObj, corr) =
            case tryPreProof of
              Left err -> (object [ "feedback" .= toJSON (displayPPPError err) ], False)
              Right pp ->
                case checkProofOf (piArgument piAttempt) pp of
                  ProofOfArgument arg ->
                        (object [ "feedback" .= toJSON (displayCheckProofOf (ProofOfArgument arg))], True)
                  oops ->
                        (object [ "feedback" .= toJSON (displayCheckProofOf oops) ], False)
          attempt = Attempt { attemptUserId = Nothing
                            , attemptExerciseType = ProofIntuitionistic
                            , attemptIsCorrect = corr
                            , attemptExerciseContent =
                                Just (pack . show . toJSON $ piArgument piAttempt)
                            , attemptSubmittedResponse =
                                Just (pack . show . toJSON $ piProof piAttempt)
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

buttonIds :: (Text, Text)
buttonIds = ("js-button-div", "js-submit-button")

otherIds :: (Text, Text, Text)
otherIds = ("js-proof-id", "js-feedback-id", "js-arg-display-id")
