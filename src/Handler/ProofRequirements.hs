{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ProofRequirements where

import ExerciseType (ExerciseType(..))
import Foundation
  ( Route(ProofRequirementsR), Handler )
import Import.NoFoundation
  ( Attempt(..)
  , Bool(..)
  , Either(..)
  , Entity(..)
  , Exercise(..)
  , FromJSON(..)
  , Html
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
import Data.List (nub)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified System.Random as SR
import Text.Julius (rawJS)

import Handler.LoginCheck (loginNotifyW)
import Handler.Proofs (singleProofEntry)
import Logic.Formulas (atom, displayFormula)
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
  PRAttempt { prExerciseId :: Key Exercise
            , prRawProof :: RawProofTree
            }

instance FromJSON PRAttempt where
  parseJSON = withObject "PRAttempt" $ \v -> PRAttempt
    <$> v .: "pExContentPost"
    <*> v .: "pExSubmittedProof"

fmSetts :: RandomFormulaSettings
fmSetts = defaultRandomFormulaSettings ProofWithRequirements

assumptionSetts :: RandomFormulaSettings
assumptionSetts = fmSetts { rfDegreeWeights = [1, 1, 1] }

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

-- | want decodePR (encodePR) == pr, for all ProofRequirements pr
decodePR :: Text -> Maybe ProofRequirements
decodePR tx = decodeStrict $ encodeUtf8 tx

encodePR :: ProofRequirements -> Text
encodePR pr = decodeUtf8 . toStrict $ encode pr

getProofRequirementsR :: Handler Html
getProofRequirementsR = do
  requirements <- liftIO $ randomRequirementsIO fmSetts assumptionSetts
  let ex = Exercise { exerciseExerciseType = ProofWithRequirements
                    , exerciseExerciseContent = encodePR requirements }
      (buttonSubmitId, buttonDivId) = buttonIds
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
        $(widgetFile "proofs-layout")
    Nothing -> defaultLayout $ do
      setTitle "Proofs in NJ"
      toWidgetHead [hamlet|<script src=/static/js/proof.js>|]
      toWidgetHead
          [hamlet|<link rel=stylesheet href=static/css/proof.css type="text/css" media="screen" title="no title" charset="utf-8">|]
      $(widgetFile "proofs-layout")

-- TODO: This is a hack, setting up fake ridiculous requirements that are (almost?) certainly failed,
-- for use when there is somehow a DB entry with requirements we can't read, which should never happen
ridiculousRequirements :: ProofRequirements
ridiculousRequirements =
  let sillyList = [atom "p"]
  in ProofRequirements { reqConclusion = atom "@#*!**#**!"
                       , reqOpenAssumptions = sillyList
                       , reqDischarged = sillyList
                       , reqMinRules = 100000
                       }

postProofRequirementsR :: Handler Value
postProofRequirementsR = do
  tryResponse <- (parseCheckJsonBody :: Handler (Result PRAttempt))
  case tryResponse of
    Error err -> returnJson err
    Success prAttempt -> do
      mex <- runDB $ getEntity (prExerciseId prAttempt)
      case mex of
        Nothing -> error "No such exercise has been given!"
        Just (Entity exid ex) -> do
          let reqs = fromMaybe ridiculousRequirements (decodePR $ exerciseExerciseContent ex)
              (responseObj, corr) = case (readPreProof . prRawProof $ prAttempt) of
                                        Left err -> (object [ "feedback" .= toJSON (displayPPPError err) ], False)
                                        Right pp ->
                                            case checkRequirements reqs pp of
                                              MeetsRequirements ->
                                                (object [ "feedback" .= toJSON (displayRequirementsCheck MeetsRequirements)], True)
                                              oops ->
                                                (object [ "feedback" .= toJSON (displayRequirementsCheck oops)], False)
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
                  returnJson (insertedAttempt, responseObj)
              Nothing -> returnJson (attempt, responseObj)

buttonIds :: (Text, Text)
buttonIds = ("js-button-submit-proof", "js-button-div")

divIds :: (Text, Text)
divIds = ("js-proof", "js-feedback")
