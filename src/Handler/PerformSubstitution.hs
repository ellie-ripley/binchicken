{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.PerformSubstitution where

import Data.Aeson (Result(..), (.=), decodeStrict, encode, object, toJSON)
import qualified Data.Map as M 
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)

import Foundation (Route(PerformSubstitutionR))
import Import
  ( Exercise(..)
  , FromJSON
  , Handler
  , Html
  , Key
  , SentExercise(..)
  , Text
  , ToJSON
  , Value
  , Widget
  , defaultLayout
  , getCurrentTime
  , insert
  , liftIO
  , lucius
  , maybeAuthId
  , runDB
  , setTitle
  , toStrict
  , toWidget
  , whamlet
  , widgetFile
  )
import Text.Julius (rawJS)

import ExerciseType (ExerciseType(PerformSubstitution))
import Handler.Common (updateScore)
import Handler.LoginCheck (loginNotifyW)
import Logic.Formulas
  ( Formula
  , GenFormula(..)
  , Substitution(..)
  , displayFormula
  )
import Logic.Random (randomFormulaIO, randomSubstitutionIO)
import Settings.Binchicken
  ( RandomFormulaSettings(..)
  , RandomSubstitutionSettings(..)
  , defaultRandomFormulaSettings
  , defRandomSubstitutionSettings
  )

data PSAttempt =
  PSAttempt { psExerciseId :: Key Exercise
            , psResponse :: Formula
            } deriving (Generic)
instance ToJSON PSAttempt
instance FromJSON PSAttempt

setts :: RandomSubstitutionSettings
setts = defRandomSubstitutionSettings

-- | want decodePS (encodePS fm sb) == Just (fm, sb), for every fm, sb
decodePS :: Text -> Maybe (Formula, Substitution)
decodePS tx = decodeStrict $ encodeUtf8 tx

encodePS :: Formula -> Substitution -> Text
encodePS fm sb = decodeUtf8 . toStrict $ encode (fm, sb)

substitutionWidget :: Substitution -> Widget
substitutionWidget (Sub subMap) = do 
  toWidget [whamlet|
                <table .table.table-striped>
                    <thead>
                        <tr>
                            <td scope="col">Atomic
                            <td scope="col"><code>s</code>(Atomic)
                    <tbody>
                      $forall (at, fm) <- M.toList subMap
                        <tr>
                                <td .oblang.subst>#{displayFormula (A at)}
                                <td .oblang.subst>#{displayFormula fm}
           |]
  toWidget [lucius|
                    td.subst { padding: 0px 20px; }
           |]

getPerformSubstitutionR :: Handler Html
getPerformSubstitutionR = do
  let displayFormulaId = "js-display-formula" :: Text
      feedbackId = "js-display-result" :: Text
      buttonDivId = "js-button-div" :: Text
      inputFormulaId = "js-input-formula" :: Text
      submitButtonId = "js-submit-button" :: Text
      (negButt, conjButt, disjButt, implButt, fumButt, vumButt) =
        ( "js-negation-button" :: Text
        , "js-conjunction-button" :: Text
        , "js-disjunction-button" :: Text
        , "js-implication-button" :: Text
        , "js-falsum-button" :: Text
        , "js-verum-button" :: Text
        )
      ajaxRoute = PerformSubstitutionR
  (formula :: Formula) <- liftIO $ randomFormulaIO (rsRfSettings setts)
  (subst :: Substitution) <- liftIO $ randomSubstitutionIO setts
  let ex = Exercise { exerciseExerciseType = PerformSubstitution
                    , exerciseExerciseContent = encodePS formula subst
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
        setTitle "Perform the given substitution on the given formula"
        $(widgetFile "perform-substitution")
    Nothing -> defaultLayout $ do
      setTitle "Perform the given substitution on the given formula"
      $(widgetFile "perform-substitution")
  

postPerformSubstitutionR :: Handler Value
postPerformSubstitutionR = undefined
