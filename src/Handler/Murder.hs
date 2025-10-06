{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Murder where

import Foundation (Route(..), Handler, Widget)
import Import.NoFoundation ( Html
                           , undefined
                           )


import Data.Aeson (FromJSON, ToJSON, Result(..), Value(..), (.:))
import Data.Aeson.Types (parseMaybe)

getMurderR :: Handler Html
getMurderR = undefined

postMurderR :: Handler Value
postMurderR = undefined
