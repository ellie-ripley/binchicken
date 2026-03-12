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
      Int,
      Yesod(defaultLayout),
      fst,
      null,
      snd,
      widgetFile,
      zip
    )
import ExerciseType (prettyExerciseName)
import Settings.Binchicken (exerciseRoute, activeExerciseTypes)


  
getExercisesR :: Handler Html
getExercisesR = do
    let exTypes = zip ([(1::Int)..]) activeExerciseTypes -- list active exercises only
    defaultLayout $ do
        setTitle "Exercises"
        $(widgetFile "exercises")
