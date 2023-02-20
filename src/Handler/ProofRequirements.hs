{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ProofRequirements where

import Import
import Data.Aeson (Result(..), withObject)
import Data.List (nub)
import qualified System.Random as SR
import Text.Julius (rawJS)

import Handler.LoginCheck (loginNotifyW)
import Handler.Proofs (singleProofEntry)
import Logic.Formulas (displayFormula)
import Logic.Random (randomFormula, randomFormulas)
import Logic.PreProofs ( RawProofTree(..)
                       , Rule(..)
                       , UnaryRule(..)
                       , TrinaryRule(..)
                       , readPreProof
                       , ruleName
                       , displayPPPError
                       )
import Logic.Proofs
import Settings.Binchicken (RandomFormulaSettings(..), defaultRandomFormulaSettings)



data PRAttempt =
  PRAttempt { prRequirements :: ProofRequirements
            , prProof :: RawProofTree
            }

instance FromJSON PRAttempt where
  parseJSON = withObject "PRAttempt" $ \v -> PRAttempt
    <$> v .: "pExContentPost"
    <*> v .: "pExSubmittedProof"

fmSetts :: RandomFormulaSettings
fmSetts = defaultRandomFormulaSettings ProofWithRequirements

assumptionSetts :: RandomFormulaSettings
assumptionSetts = fmSetts { rfMaxDegree = 2 }

randomRequirements
  :: forall g. (SR.RandomGen g)
  => RandomFormulaSettings   --for the required conclusion
  -> RandomFormulaSettings   --for any required assumptions
  -> g
  -> (ProofRequirements, g)
randomRequirements cSetts aSetts g =
  let (con, g1) = randomFormula cSetts g
      (size, g2) = SR.randomR (1, 4) g1
      (reqAs, g3) = SR.randomR (0, 2) g2
      (reqDs, g4) = SR.randomR (0, 2) g3
      (fms, g5) = randomFormulas aSetts (reqAs + reqDs) g4
      (rOA, rDA) = splitAt reqAs fms
      prfR = ProofRequirements { reqConclusion = con
                               , reqOpenAssumptions = nub rOA
                               , reqDischarged = nub rDA
                               , reqMinRules = size
                               }
  in (prfR, g5)


randomRequirementsIO
  :: RandomFormulaSettings --conclusion
  -> RandomFormulaSettings --assumption
  -> IO ProofRequirements
randomRequirementsIO cSetts aSetts = SR.getStdRandom $ randomRequirements cSetts aSetts

getProofRequirementsR :: Handler Html
getProofRequirementsR = do
  requirements <- liftIO $ randomRequirementsIO fmSetts assumptionSetts
  let (buttonSubmitId, buttonDivId) = buttonIds
      (proofIdPrefix, feedbackId) = divIds
      proofId = proofIdPrefix <> "1"
      startingConclusion = reqConclusion requirements
      pExName = "Proof with requirements" :: Text
      pExInstructions =
        [whamlet|<p>Enter an NJ proof meeting these requirements. (You might want to work this out on scratch paper first!)|]
      pExContent =
        [whamlet|<ul>
                   <li>Has the conclusion <code .oblang>#{displayFormula $ reqConclusion requirements}</code>
                   $if null (reqOpenAssumptions requirements)
                   $else
                      <li>Includes as open assumption(s):
                        <ul>
                            $forall oa <- (reqOpenAssumptions requirements)
                                <li><code .oblang>#{displayFormula oa}</code>
                   $if null (reqDischarged requirements)
                   $else
                      <li>Includes as discharged assumption(s):
                        <ul>
                            $forall da <- (reqDischarged requirements)
                                <li><code .oblang>#{displayFormula da}</code>
                   $if reqMinRules requirements == 1
                      <li>Uses at least one rule
                   $if reqMinRules requirements > 1
                      <li>Uses at least #{show $ reqMinRules requirements} rules
        |]
      pExEntry = singleProofEntry proofId ""
      pExContentPost = toJSON requirements
      ajaxRoute = ProofRequirementsR
  defaultLayout $ do
    setTitle "Proofs in NJ"
    toWidgetHead [hamlet|<script src=/static/js/proof.js>|]
    toWidgetHead
          [hamlet|<link rel=stylesheet href=static/css/proof.css type="text/css" media="screen" title="no title" charset="utf-8">|]
    $(widgetFile "proofs-layout")


postProofRequirementsR :: Handler Value
postProofRequirementsR = do
  tryResponse <- (parseCheckJsonBody :: Handler (Result PRAttempt))
  case tryResponse of
    Error err -> returnJson err
    Success prAttempt -> do
      let tryPreProof = readPreProof . prProof $ prAttempt
          (responseObj, corr) =
              case tryPreProof of
                Left err -> (object [ "feedback" .= toJSON (displayPPPError err) ], False)
                Right pp ->
                    case checkRequirements (prRequirements prAttempt) pp of
                        MeetsRequirements ->
                            (object [ "feedback" .= toJSON (displayRequirementsCheck MeetsRequirements)], True)
                        oops ->
                                (object [ "feedback" .= toJSON (displayRequirementsCheck oops)], False)
          attempt = Attempt { attemptUserId = Nothing
                            , attemptExerciseType = ProofWithRequirements
                            , attemptIsCorrect = corr
                            , attemptExerciseContent = Just (pack . show . toJSON $ prRequirements prAttempt)
                            , attemptSubmittedResponse = Just (pack .  show . toJSON $ prProof prAttempt)
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
buttonIds = ("js-button-submit-proof", "js-button-div")

divIds :: (Text, Text)
divIds = ("js-proof", "js-feedback")
