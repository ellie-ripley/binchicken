{-# LANGUAGE OverloadedStrings #-}

module BinEmail where

import Import.NoFoundation (fromString)

import Data.Text (Text, unpack)
import System.Environment (getEnv)
import System.Process (callCommand)

mailgunDomain :: Text
mailgunDomain = "mail.binchicken.one"

mailgunApiKey :: IO Text
mailgunApiKey = fromString <$> getEnv "BINCHICKEN_MAILGUN_API"

verEmailBody
  :: Text -- ^ the verification url
  -> Text -- ^ the full text of the email to be sendStatusJSON
verEmailBody verurl = "Please confirm your email address by clicking on this link: "
                        <> verurl <>
                        " Thanks!"

binSendVerifyEmail
  :: Text -- ^ email address to send to
  -> Text -- ^ subject of email
  -> Text -- ^ verification url to include
  -> IO ()
binSendVerifyEmail em sub vu = do
  mak <- mailgunApiKey
  let cmd = "curl -s --user 'api:"
                <> mak
                <> "'"
                <> " https://api.mailgun.net/v3/"
                <> mailgunDomain
                <> "/messages"
                <> " -F from='noreply <noreply@"
                <> mailgunDomain
                <> ">'"
                <> " -F to="
                <> em
                <> " -F subject='"
                <> sub
                <> "'"
                <> " -F text='"
                <> verEmailBody vu
                <> "'"

  callCommand $ unpack cmd
