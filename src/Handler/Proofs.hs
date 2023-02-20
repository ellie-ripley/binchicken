{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Proofs where

import Data.Text (Text)

import Import ( Widget
              , whamlet
              )


singleProofEntry
  :: Text -- ^ prefix of id of resulting div, to be targeted by proofJS
  -> Text -- ^ Optional header text
  -> Widget
singleProofEntry pid hd =
  [whamlet|
      <div .row>
         <div .col-lg-12>
             $if hd /= ""
                <h5>#{hd}
             <code>
                 <div .proofcontainer>
                     <div ##{pid}>
  |]
