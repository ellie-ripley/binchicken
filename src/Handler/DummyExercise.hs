{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.DummyExercise where

import Import
import Text.Julius (RawJS (..))

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getDummyExerciseR :: Handler Html
getDummyExerciseR = do
    let (buttonCorrectId, buttonIncorrectId, displayResultId) = buttonIds
    defaultLayout $ do
        setTitle "Dummy exercise"
        $(widgetFile "dummy-exercise")

postDummyExerciseR :: Handler Value
postDummyExerciseR = do
    attempt <- (requireCheckJsonBody :: Handler Attempt)
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
        Just uid -> do
            now <- liftIO getCurrentTime
            let attempt' = attempt { attemptUserId = Just uid, attemptSubmittedAt = Just now }
            insertedAttempt <- runDB $ insertEntity attempt'
            returnJson insertedAttempt
        Nothing -> returnJson attempt

buttonIds :: (Text, Text, Text)
buttonIds = ("js-button-correct", "js-button-incorrect", "js-display-result")
