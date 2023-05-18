{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ProofNormalize where

import Import ( defaultLayout
              , getCurrentTime
              , hamlet
              , insertEntity
              , liftIO
              , maybeAuthId
              , parseCheckJsonBody
              , returnJson
              , runDB
              , setTitle
              , toWidgetHead
              , whamlet
              , widgetFile
              , Attempt(..)
              , Handler
              , Html
              , Route(..)
              )

import Data.Aeson ( FromJSON(..)
                  , Result(..)
                  , Value(..)
                  , object
                  , toJSON
                  , withObject
                  , (.=)
                  , (.:)
                  )
import Data.Text (Text, pack)
import qualified System.Random as SR
import Text.Julius (rawJS)

import Logic.PreProofs
    ( Rule(RT, RU),
      TrinaryRule(DE),
      UnaryRule(II, NI),
      RawProofTree,
      ruleName,
      displayPPPError,
      readPreProof )

import ExerciseType (ExerciseType(..))
import Handler.LoginCheck (loginNotifyW)
import Handler.Proofs (singleProofEntry)
import Settings.Binchicken ( defNormalizeRequirementSettings
                           , NormalizeRequirementSettings(..)
                           )
import Logic.Formulas ( displayFormula
                      )
import Logic.Proofs ( checkNormalized
                    , displayCheckNormalized
                    , maxSegments
                    , renderMaxSegments
                    , renderPNRequirements
                    , CheckNormalized(..)
                    , PNRequirements(..)
                    )
import Logic.Random ( randomFormula )


data PNAttempt =
  PNAttempt { pnRequirements :: PNRequirements
            , pnOriginalProof :: RawProofTree
            , pnNormalizedProof :: RawProofTree
            }

instance FromJSON PNAttempt where
  parseJSON = withObject "PNAttempt" $ \v -> PNAttempt
    <$> v .: "pExContentPost"
    <*> v .: "pExSubmittedProof"
    <*> v .: "pExSecondProof"


randomElem
  :: (SR.RandomGen g)
  => [a]
  -> g
  -> (a, g)
randomElem xs g
  | length xs < 1 = error "Can't take an element of an empty list!"
  | otherwise =
      let (ix, h) = SR.randomR (0, length xs - 1) g
      in (xs !! ix, h)

randomNormalizeReqs
  :: (SR.RandomGen g)
  => NormalizeRequirementSettings
  -> g
  -> (PNRequirements, g)
randomNormalizeReqs setts g
  | nrMaxLength setts < nrMinLength setts = error "Badly formed settings!"
  | nrMaxLength setts < 0 = error "Awful settings! Don't do that!"
  | otherwise =
      let (conn, g1) = randomElem (nrConnectives setts) g
          (concl, g2)  = randomFormula (nrrfConclusion setts) g1
          (len, g3)    = SR.randomR (nrMinLength setts, nrMaxLength setts) g2
          reqs = PNRequirements { pnConclusion = concl
                                , pnMainConnective = conn
                                , pnSegmentLength = len
                                }
      in (reqs, g3)

randomNormalizeReqsIO :: NormalizeRequirementSettings -> IO PNRequirements
randomNormalizeReqsIO = SR.getStdRandom . randomNormalizeReqs


nrSetts :: NormalizeRequirementSettings
nrSetts = defNormalizeRequirementSettings

getProofNormalizeR :: Handler Html
getProofNormalizeR = do
  nrReqs <- liftIO $ randomNormalizeReqsIO nrSetts
  let (buttonDivId, buttonSubmitId) = buttonIds
      (proofIdPrefix, feedbackId) = otherIds
      proofId = proofIdPrefix <> "1"
      secondProofId = proofIdPrefix <> "2"
      startingConclusion = pnConclusion nrReqs
      pExName = "Proof normalization" :: Text
      pExInstructions =
        [whamlet|<p>Give two proofs below. Your first proof should meet the given requirements, and your second proof should be in normal form and have no open assumptions that are not also open in the first proof. The best way to do this is to make your first proof and then normalize it, inputting the normal form as the second proof; that will always work.

            <p>Note that if you want the second proof to be more than just an assumption of the conclusion, you will need to add any extra nodes yourself. |]
      pExContent = renderPNRequirements nrReqs
      pExEntry =
        [whamlet|
                ^{singleProofEntry proofId "Original proof:"}
                ^{singleProofEntry secondProofId "Normalized proof:"}
        |]
      ajaxRoute = ProofNormalizeR
      pExContentPost = toJSON nrReqs
  defaultLayout $ do
    setTitle "Normalization in NJ"
    toWidgetHead [hamlet|<script src=/static/js/proof.js>|]
    toWidgetHead
        [hamlet|<link rel=stylesheet href=static/css/proof.css type="text/css" media="screen" charset="utf-8">|]
    $(widgetFile "proofs-layout")

postProofNormalizeR :: Handler Value
postProofNormalizeR = do
  tryResponse <- (parseCheckJsonBody :: Handler (Result PNAttempt))
  case tryResponse of
    Error err -> returnJson err
    Success pnAttempt -> do
      let tryOrigPreProof = readPreProof . pnOriginalProof $ pnAttempt
          tryNormPreProof = readPreProof . pnNormalizedProof $ pnAttempt
          (responseObj, corr) =
            case (tryOrigPreProof, tryNormPreProof) of
              (Left err, _) -> (object [ "feedback" .= toJSON ("Error in original proof: " <> displayPPPError err) ], False)
              (_, Left err) -> (object [ "feedback" .= toJSON ("Error in normalized proof: " <> displayPPPError err) ], False)
              (Right opp, Right npp) ->
                let fb = checkNormalized (pnRequirements pnAttempt) opp npp
                in case fb of
                  CorrectAndNormalized ->
                        (object [ "feedback" .= toJSON  (displayCheckNormalized fb <> (renderMaxSegments . maxSegments $ opp)) ], True)
                  _ ->
                        (object [ "feedback" .= toJSON  (displayCheckNormalized fb <> (renderMaxSegments . maxSegments $ opp)) ], False)
          attempt = Attempt { attemptUserId = Nothing
                            , attemptExerciseType = ProofNormalize
                            , attemptIsCorrect = corr
                            , attemptExerciseContent =
                                Just (pack . show . toJSON $ pnRequirements pnAttempt)
                            , attemptSubmittedResponse =
                                Just (pack . show . toJSON $ [pnOriginalProof pnAttempt, pnNormalizedProof pnAttempt])
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

otherIds :: (Text, Text)
otherIds = ("js-proof-id", "js-feedback-id")
