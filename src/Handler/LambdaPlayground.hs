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
              , shamlet
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
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Julius (rawJS)

import Logic.Lambdas.Types (LVar(..), lvarList)
import Logic.Lambda   ( dbIsRedex
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
                let dbt = deBruijn tm
                    aeq = namify (lvarList ['a'..'z']) dbt
                    normalDB = normaliseDB dbt
                    normaltm = namify (lvarList ['a'..'z']) normalDB
                    tminfo = [shamlet|<ul>
                                         <li>With all parentheses: #{displayTermAllPars tm}
                                         <li>With minimal parentheses: #{displayTermMinPars tm}
                                         $if null (freeVars tm)
                                            <li>It has no free variables
                                         $else
                                            <li>Its free variables:
                                              $forall vr <- freeVars tm
                                                 <code> #{displayLVar vr}
                                         $maybe a <- aeq
                                           <li>An alpha equivalent term: #{displayTermMinPars a}
                                         $nothing
                                           <li>Couldn't make an alpha equivalent: that's a big term!
                                         <li>Locally nameless (ignore this!): #{displayDeBruijn dbt}
                                         <li>#{displayDBIsRedex (dbIsRedex dbt)}
                                         <li>#{displayDBReductionResult (dbParallelOneStep dbt)}
                                         <li>Normal form: #{displayDeBruijn normalDB}
                                         $maybe t <- normaltm
                                           <li>Or: #{displayTerm t}
                                         $nothing
                                           <li>Couldn't namify: that's a big term!
                                         <li>Printed: #{displayTerm tm}
                             |]
                in  object [ "feedback" .= toJSON ("Good term!" :: Text)
                           , "termInfo" .= (toJSON . renderHtml $ tminfo)
                           ]
      returnJson responseObj

buttonIds :: (Text, Text, Text)
buttonIds = ("js-button-div", "js-submit-button", "js-lambda-button")

otherIds :: (Text, Text)
otherIds = ("js-lambda-id", "js-feedback-id")
