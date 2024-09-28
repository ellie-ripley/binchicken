{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.BetaReduction where

import Import ( defaultLayout
              , fromMaybe
              , getCurrentTime
              , getEntity
              , insert
              , insertEntity
              , liftIO
              , maybeAuthId
              , parseCheckJsonBody
              , returnJson
              , runDB
              , setTitle
              , toStrict
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
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Text.Julius (rawJS)

import ExerciseType (ExerciseType(..))
import Handler.Common (updateScore)
import Logic.Lambdas.Types (LVar(..), Term(..), lvarList)
import Logic.Lambda   ( deBruijn
                      , displayTerm
                      , normaliseDB
                      , parseTerm
                      )
import Logic.Random (randomLambdaTermIO)
import Scoring (Correct(..), correctToBool)

data BRAttempt =
  BRAttempt { brExerciseId :: Key Exercise
            , brResponse :: Text
            }
    deriving (Generic, ToJSON, FromJSON)


-- | want decodeAE (encodeAE tm) == Just tm, for every term
decodeBR :: Text -> Maybe Term
decodeBR tx = decodeStrict $ encodeUtf8 tx

encodeBR :: Term -> Text
encodeBR tm = decodeUtf8 . toStrict $ encode tm


getBetaReductionR :: Handler Html
getBetaReductionR = do
  let (buttonDivId, buttonSubmitId, lambdaButt, normalButt) = buttonIds
      (termId, feedbackId) = otherIds
      ajaxRoute = BetaReductionR
  leftTerm <- liftIO $ randomLambdaTermIO (lvarList ['t'..'z']) (0,2)
  rightTerm <- liftIO $ randomLambdaTermIO (lvarList ['t'..'z']) (0,2)
  let targetTerm = TApp leftTerm rightTerm
      ex = Exercise { exerciseExerciseType = BetaReduction
                    , exerciseExerciseContent = encodeBR targetTerm
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
        setTitle "Beta reduction"
        $(widgetFile "beta-reduction")
    Nothing -> defaultLayout $ do
      setTitle "Beta reduction"
      $(widgetFile "beta-reduction")

data BetaRedFeedback
  = BRCorrect
  | BRNotATerm
  | BRCantReach
  | BRNotNormal
  deriving (Eq, Show)

displayBRF :: BetaRedFeedback -> Text
displayBRF = \case
  BRCorrect -> "Right on!"
  BRNotATerm -> "Your submission is not a correctly-formed term."
  BRCantReach -> "Your term can't be reached by beta reduction from the given term."
  BRNotNormal -> "There's more reduction left to do!"

brIsCorr :: BetaRedFeedback -> Correct
brIsCorr = \case
  BRCorrect -> Correct
  _         -> Incorrect

markBR :: Term -> Text -> BetaRedFeedback
markBR given submitted =
  case parseTerm submitted of
    Left _ -> BRNotATerm
    Right tm ->
      let targ = normaliseDB (deBruijn given)
          subm = deBruijn tm
      in if targ == subm
         then BRCorrect
         else if targ == normaliseDB subm
              then BRNotNormal
              else BRCantReach


postBetaReductionR :: Handler Value
postBetaReductionR = do
  tryResponse <- (parseCheckJsonBody :: Handler (Result BRAttempt))
  case tryResponse of
    Error err -> returnJson err
    Success brAttempt -> do
      mex <- runDB $ getEntity (brExerciseId brAttempt)
      case mex of
        Nothing -> error "No such exercise has been generated!"
        Just (Entity exid ex) -> do
                let orig = fromMaybe (TVar $ LVar "Impossible") (decodeBR $ exerciseExerciseContent ex)
                    brf = markBR orig (brResponse brAttempt)
                    responseObj = object [ "feedback" .= toJSON (displayBRF brf) ]
                    corr = brIsCorr brf
                    attempt = Attempt { attemptUserId = Nothing
                                      , attemptExerciseId = exid
                                      , attemptIsCorrect = correctToBool corr
                                      , attemptSubmittedResponse = Just (brResponse brAttempt)
                                      , attemptSubmittedAt = Nothing
                                      }
                maybeCurrentUserId <- maybeAuthId
                case maybeCurrentUserId of
                  Just uid -> do
                    now <- liftIO getCurrentTime
                    let attempt' = attempt { attemptUserId = Just uid, attemptSubmittedAt = Just now }
                    insertedAttempt <- runDB $ insertEntity attempt'
                    updateScore uid BetaReduction corr
                    returnJson (insertedAttempt, responseObj)
                  Nothing -> returnJson (attempt, responseObj)

buttonIds :: (Text, Text, Text, Text)
buttonIds = ("js-button-div", "js-submit-button", "js-lambda-button", "js-normal-button")

otherIds :: (Text, Text)
otherIds = ("js-lambda-id", "js-feedback-id")
