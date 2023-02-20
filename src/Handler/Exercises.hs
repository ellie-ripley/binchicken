{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Exercises where

import Foundation ( Handler )
import Import.NoFoundation
    ( ($),
      setTitle,
      Html,
      Yesod(defaultLayout),
      widgetFile,
      null,
      Enum(..) )
import ExerciseType (prettyExerciseName)
import Settings.Binchicken (exerciseRoute, activeExerciseTypes)


-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getExercisesR :: Handler Html
getExercisesR = do
    let exTypes = activeExerciseTypes -- list active exercises only
    defaultLayout $ do
        setTitle "Exercises"
        $(widgetFile "exercises")
