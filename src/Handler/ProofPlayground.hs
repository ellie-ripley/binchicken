{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ProofPlayground where

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
    ( Rule(RT, RU),
      TrinaryRule(DE),
      UnaryRule(II, NI),
      RawProofTree,
      ruleName,
      displayPPPError,
      makeRaw,
      ppConclusion,
      readPreProof )

import Handler.Proofs (singleProofEntry)
import Logic.Formulas ( displayFormula
                      )
import Logic.Proofs ( maxSegments
                    , renderMaxSegments
                    , removeCompoundFEs
                    )


data PPData = PPData { submittedProofTree :: RawProofTree }
  deriving (Generic, ToJSON, FromJSON)



getProofPlaygroundR :: Handler Html
getProofPlaygroundR = do
  let (buttonDivId, buttonSubmitId, buttonChangeConclusionId) = buttonIds
      (proofIdPrefix, feedbackId, newConclusionId) = otherIds
      proofId = proofIdPrefix <> "1"
      secondProofId = proofIdPrefix <> "2"
      ajaxRoute = ProofPlaygroundR
  defaultLayout $ do
    setTitle "Proof playground"
    toWidgetHead [hamlet|<script src=/static/js/proof.js>|]
    toWidgetHead
        [hamlet|<link rel=stylesheet href=static/css/proof.css type="text/css" media="screen" charset="utf-8">|]
    $(widgetFile "proof-playground")

postProofPlaygroundR :: Handler Value
postProofPlaygroundR = do
  tryResponse <- (parseCheckJsonBody :: Handler (Result PPData))
  case tryResponse of
    Error err -> returnJson err
    Success ppData-> do
      let tryOrigPreProof = readPreProof . submittedProofTree $ ppData
          responseObj =
            case tryOrigPreProof of
              Left err -> object [ "feedback" .= toJSON ("Error in original proof: " <> displayPPPError err) ]
              Right prf ->
                let mSegs = maxSegments prf
                    maxFeed = renderMaxSegments mSegs
                    concFeed = "The conclusion is " <> displayFormula (ppConclusion prf)
                    fb = unpack concFeed <> maxFeed
                    returnPrf = removeCompoundFEs prf
                in object [ "feedback" .= toJSON fb
                          , "newproof" .= toJSON (makeRaw returnPrf)
                          ]
      returnJson responseObj

buttonIds :: (Text, Text, Text)
buttonIds = ("js-button-div", "js-submit-button", "js-change-conclusion-button")

otherIds :: (Text, Text, Text)
otherIds = ("js-proof-id", "js-feedback-id", "js-new-conclusion-id")
