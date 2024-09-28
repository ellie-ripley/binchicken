{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.AlphaEquivalence where

import Import ( defaultLayout
              , fromMaybe
              , getCurrentTime
              , getEntity
              , hamlet
              , insert
              , insertEntity
              , liftIO
              , maybeAuthId
              , parseCheckJsonBody
              , returnJson
              , runDB
              , shamlet
              , setTitle
              , toStrict
              , toWidgetHead
              , widgetFile
              , Attempt(..)
              , Entity(..)
              , Exercise(..)
              , Handler
              , Html
              , Key
              , Route(..)
              , SentExercise(..)
              )

import Data.Aeson ( FromJSON
                  , ToJSON
                  , Result(..)
                  , Value(..)
                  , decodeStrict
                  , encode
                  , object
                  , toJSON
                  , (.=)
                  )
import Data.List (intersect)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Julius (rawJS)

import ExerciseType (ExerciseType(..))
import Handler.Common (updateScore)
import Logic.Lambdas.Types (LVar(..), Term(..), lvarList)
import Logic.Lambda   ( boundVars
                      , dbIsRedex
                      , dbParallelOneStep
                      , deBruijn
                      , displayDBIsRedex
                      , displayDBReductionResult
                      , displayDeBruijn
                      , displayLVar
                      , displayTerm
                      , displayTermAllPars
                      , displayTermMinPars
                      , displayTermError
                      , namify
                      , normaliseDB
                      , parseTerm
                      , freeVars
                      )
import Logic.Random (randomLambdaTermIO)
import Scoring (Correct(..), correctToBool)

data AEAttempt =
  AEAttempt { aeExerciseId :: Key Exercise
            , aeResponse :: Text
            }
    deriving (Generic, ToJSON, FromJSON)


-- | want decodeAE (encodeAE tm) == Just tm, for every term
decodeAE :: Text -> Maybe Term
decodeAE tx = decodeStrict $ encodeUtf8 tx

encodeAE :: Term -> Text
encodeAE tm = decodeUtf8 . toStrict $ encode tm


getAlphaEquivalenceR :: Handler Html
getAlphaEquivalenceR = do
  let (buttonDivId, buttonSubmitId, lambdaButt) = buttonIds
      (termId, feedbackId) = otherIds
      ajaxRoute = AlphaEquivalenceR
  targetTerm <- liftIO $ randomLambdaTermIO (lvarList ['t'..'z']) (1,4)
  let ex = Exercise { exerciseExerciseType = AlphaEquivalence
                    , exerciseExerciseContent = encodeAE targetTerm
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
        setTitle "Alpha equivalence"
        $(widgetFile "alpha-equivalence")
    Nothing -> defaultLayout $ do
      setTitle "Alpha equivalence"
      $(widgetFile "alpha-equivalence")

data AlphaEqFeedback
  = AECorrect
  | AENotATerm
  | AENotAlphaEq
  | AEBoundVarOverlap
  deriving (Eq, Show)

displayAEF :: AlphaEqFeedback -> Text
displayAEF = \case
  AECorrect -> "Right on!"
  AENotATerm -> "Your submission is not a correctly-formed term."
  AENotAlphaEq -> "Your term is not alpha equivalent to the given term."
  AEBoundVarOverlap -> "Your term is alpha equivalent to the given term, but uses a bound variable that was bound in the given term."

aeIsCorr :: AlphaEqFeedback -> Correct
aeIsCorr = \case
  AECorrect -> Correct
  _         -> Incorrect

markAE :: Term -> Text -> AlphaEqFeedback
markAE given submitted =
  case parseTerm submitted of
    Left _ -> AENotATerm
    Right tm
      | deBruijn given /= deBruijn tm -> AENotAlphaEq
      | not . null $ intersect (boundVars given) (boundVars tm) -> AEBoundVarOverlap
      | otherwise -> AECorrect

postAlphaEquivalenceR :: Handler Value
postAlphaEquivalenceR = do
  tryResponse <- (parseCheckJsonBody :: Handler (Result AEAttempt))
  case tryResponse of
    Error err -> returnJson err
    Success aeAttempt -> do
      mex <- runDB $ getEntity (aeExerciseId aeAttempt)
      case mex of
        Nothing -> error "No such exercise has been generated!"
        Just (Entity exid ex) -> do
                let orig = fromMaybe (TVar $ LVar "Impossible") (decodeAE $ exerciseExerciseContent ex)
                    aef = markAE orig (aeResponse aeAttempt)
                    responseObj = object [ "feedback" .= toJSON (displayAEF aef) ]
                    corr = aeIsCorr aef
                    attempt = Attempt { attemptUserId = Nothing
                                      , attemptExerciseId = exid
                                      , attemptIsCorrect = correctToBool corr
                                      , attemptSubmittedResponse = Just (aeResponse aeAttempt)
                                      , attemptSubmittedAt = Nothing
                                      }
                maybeCurrentUserId <- maybeAuthId
                case maybeCurrentUserId of
                  Just uid -> do
                    now <- liftIO getCurrentTime
                    let attempt' = attempt { attemptUserId = Just uid, attemptSubmittedAt = Just now }
                    insertedAttempt <- runDB $ insertEntity attempt'
                    updateScore uid AlphaEquivalence corr
                    returnJson (insertedAttempt, responseObj)
                  Nothing -> returnJson (attempt, responseObj)

buttonIds :: (Text, Text, Text)
buttonIds = ("js-button-div", "js-submit-button", "js-lambda-button")

otherIds :: (Text, Text)
otherIds = ("js-lambda-id", "js-feedback-id")
