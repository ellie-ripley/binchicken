{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.TimeMachine where

import Foundation ( Route(..), Handler, Widget )
import Import.NoFoundation
    ( fst,
      snd,
      ($),
      AForm,
      Bounded(..),
      Either(..),
      Entity(..),
      Eq((==)),
      Exercise(..),
      FormResult(..),
      Generic,
      Int,
      Key,
      Maybe(..),
      MForm,
      MonadIO(liftIO),
      Num(..),
      FromJSON,
      SelectOpt(..),
      SentExercise(..),
      Show(show),
      TimeWordle(..),
      ToJSON(toJSON),
      Text,
      Value,
      (++),
      (<$>),
      (<*>),
      (<),
      (>),
      (&&&),
      (==.),
      areq,
      check,
      error,
      fromMaybe,
      hamlet,
      map,
      pack,
      getEntity,
      id,
      insert,
      insertEntity,
      intField,
      generateFormPost,
      getCurrentTime,
      lucius,
      mempty,
      otherwise,
      parseCheckJsonBody,
      redirect,
      renderTable,
      return,
      returnJson,
      runFormPost,
      selectFieldList,
      selectList,
      setTitle,
      textField,
      toStrict,
      toWidget,
      whamlet,
      widgetFile,
      (.),
      Html,
      Yesod(defaultLayout),
      YesodPersist(runDB),
      Attempt(..),
      YesodAuth(maybeAuthId) )
import Data.Text (unpack)
import Text.Read (readMaybe)

import Handler.LoginCheck (loginNotifyW)
import Model (EntityField(..))
import TimeMachine





data TimeAttemptForm =
  TimeAttemptForm { tafLogician :: Logician
                  , tafGuess :: Text
                  } deriving (Generic, ToJSON, FromJSON)

data TimeAttempt =
  TimeAttempt { taLogician :: Logician
              , taGuess :: Coords
              } deriving (Generic, ToJSON, FromJSON)

timeAttempt :: TimeAttemptForm -> Maybe TimeAttempt
timeAttempt (TimeAttemptForm log tx) = do
  n  <- readMaybe (unpack tx) :: Maybe Int
  cs <- coordsFromInt n
  return (TimeAttempt log cs)

timeAForm :: AForm Handler TimeAttemptForm
timeAForm = TimeAttemptForm
  <$> areq (selectFieldList logicians) "Logician: " Nothing
  <*> areq comboField "Time Coordinates: " Nothing
  where
    comboField = check validateCombo textField

    logicians :: [(Text, Logician)]
    logicians = map (displayLogician &&& id) [minBound..maxBound]

    validateCombo :: Text -> Either Text Text
    validateCombo tx =
      case (readMaybe $ unpack tx :: Maybe Int) of
        Nothing -> Left "That's not a number!"
        Just n
          | n < 0 -> Left "Positive numbers only"
          | n > 99999 -> Left "Just five digits, please!"
          | otherwise -> Right tx

timeForm :: Html -> MForm Handler (FormResult TimeAttemptForm, Widget)
timeForm = renderTable timeAForm


cText :: Text
cText = "correct"
iText :: Text
iText = "incorrect"

corrClass :: Int -> Logician -> Coords -> Text
corrClass n log (a, b, c, d, e) =
  let (v, w, x, y, z) = combo log
  in case n of
       1 -> chk v a
       2 -> chk w b
       3 -> chk x c
       4 -> chk y d
       5 -> chk z e
       _ -> ""
  where
    chk :: Int -> Int -> Text
    chk l m
      | l == m    = cText
      | otherwise = iText

attemptRow :: TimeAttempt -> Widget
attemptRow (TimeAttempt log cs@(a, b, c, d, e)) = do
  toWidget
    [hamlet|<tr class=#{show log}>
              <td class="log-name">#{displayLogicianShort log}
              <td class=#{corrClass 1 log cs}>#{show a}
              <td class=#{corrClass 2 log cs}>#{show b}
              <td class=#{corrClass 3 log cs}>#{show c}
              <td class=#{corrClass 4 log cs}>#{show d}
              <td class=#{corrClass 5 log cs}>#{show e}
           |]
  toWidget
    [lucius| td { font-size: 2rem;
                  border-radius: 5px;
                  padding: 5px;
                }
             .log-name { font-size: 1.5rem; }
             .#{cText} { background-color: green; }
             .#{iText} { background-color: white; }
           |]

attemptRow' :: Entity TimeWordle -> Widget
attemptRow' (Entity _ tw) = attemptRow ta
  where
    ta = TimeAttempt { taLogician = timeWordleLogician tw
                     , taGuess = fromMaybe (0,0,0,0,0) $ coordsFromInt (timeWordleGuess tw)
                     }

attemptTable :: [Entity TimeWordle] -> Widget
attemptTable tas = do
  [whamlet|<table>
             $forall ta <- tas
                ^{attemptRow' ta}
          |]
  toWidget
    [lucius|table { border-spacing: 5px;
                    border-collapse: separate;
                  }
           |]

getTimeMachineR :: Handler Html
getTimeMachineR = do
    (widg, enctype) <- generateFormPost timeForm
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
      Nothing -> redirect HomeR
      Just uid -> do
        muser <- runDB $ getEntity uid
        case muser of
          Nothing -> error "Logged in as a nonexistent user? This is a bug in the site!"
          Just _ -> do
            guesses <- runDB $ selectList [TimeWordleUserId ==. uid]
                                          [Desc TimeWordleGuessedAt]
            let history = attemptTable guesses
            defaultLayout $ do
              setTitle "TIME MACHINE"
              $(widgetFile "time-machine")

postTimeMachineR :: Handler Html
postTimeMachineR = do
    ((res, widg), enctype) <- runFormPost timeForm
    case res of
      FormMissing -> redirect TimeMachineR
      FormFailure _ -> redirect TimeMachineR
      FormSuccess taf ->
        case timeAttempt taf of
          Nothing -> redirect TimeMachineR
          Just ta -> do
              maybeCurrentUserId <- maybeAuthId
              case maybeCurrentUserId of
                  Just uid -> do
                      now <- liftIO getCurrentTime
                      let guess = TimeWordle { timeWordleUserId = uid
                                             , timeWordleLogician = taLogician ta
                                             , timeWordleGuess = intFromCoords (taGuess ta)
                                             , timeWordleGuessedAt = Just now
                                             }
                      _ <- runDB $ insertEntity guess
                      guesses <- runDB $ selectList [TimeWordleUserId ==. uid]
                                                    [Desc TimeWordleGuessedAt]
                      let history = attemptTable guesses
                      defaultLayout $ do
                        setTitle "TIME MACHINE POST"
                        $(widgetFile "time-machine")
                  Nothing -> return mempty

displayResultId :: Text
displayResultId = "js-results"
