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
      map,
      null,
      snd,
      widgetFile,
      zip
    )
import Settings.Binchicken (activeExerciseTypes, exerciseRoute, renderAET)


  
getExercisesR :: Handler Html
getExercisesR = do
    let exTypes = zip ([(1::Int)..]) (map renderAET activeExerciseTypes) -- list active exercises only
    defaultLayout $ do
        setTitle "Exercises" 
        $(widgetFile "exercises")
