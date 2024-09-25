{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.LambdaPlayground where

import Import ( defaultLayout
              , hamlet
              , parseCheckJsonBody
              , returnJson
              , setTitle
              , toWidgetHead
              , widgetFile
              , Handler
              , Html
              , Route(..)
              )

import Data.Aeson ( FromJSON
                  , ToJSON
                  , Result(..)
                  , Value(..)
                  , object
                  , toJSON
                  , (.=)
                  )
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Text.Julius (rawJS)

import Logic.PreProofs
    ( Rule(..),
      TrinaryRule(..),
      UnaryRule(..),
      BinaryRule(..),
      RawProofTree,
      ruleName,
      displayPPPError,
      makeRaw,
      ppConclusion,
      readPreProof )

import Logic.Formulas ( displayFormula
                      )
import Logic.Lambda   ( displayTermError
                      , parseTerm
                      )

data LPData = LPData { submittedTerm :: Text }
  deriving (Generic, ToJSON, FromJSON)



getLambdaPlaygroundR :: Handler Html
getLambdaPlaygroundR = do
  let (buttonDivId, buttonSubmitId, lambdaButt) = buttonIds
      (termId, feedbackId) = otherIds
      ajaxRoute = LambdaPlaygroundR
  defaultLayout $ do
    setTitle "Lambda playground"
    $(widgetFile "lambda-playground")

postLambdaPlaygroundR :: Handler Value
postLambdaPlaygroundR = do
  tryResponse <- (parseCheckJsonBody :: Handler (Result LPData))
  case tryResponse of
    Error err -> returnJson err
    Success ppData-> do
      let tryTerm =  parseTerm . submittedTerm $ ppData
          responseObj =
            case tryTerm of
              Left err -> object [ "feedback" .= toJSON ("Error in input term: " <> displayTermError err) ]
              Right tm ->
                object [ "feedback" .= toJSON ("Good term!" :: Text)
                       , "term" .= toJSON ("Here is the term it is" :: Text)
                       ]
      returnJson responseObj

buttonIds :: (Text, Text, Text)
buttonIds = ("js-button-div", "js-submit-button", "js-lambda-button")

otherIds :: (Text, Text)
otherIds = ("js-lambda-id", "js-feedback-id")
