{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module BinEmail where

import Import.NoFoundation ( Auth
                           , AuthHandler
                           , FieldSettings(..)
                           , Html
                           , Route
                           , SomeMessage(..)
                           , WidgetFor
                           , authLayout
                           , emailField
                           , fromString
                           , fvInput
                           , generateFormPost
                           , getRouteToParent
                           , getYesod
                           , languages
                           , mreq
                           , passwordField
                           , renderAuthMessage
                           , setTitleI
                           , whamlet
                           )

import Data.Text (Text, unpack)
import System.Environment (getEnv)
import System.Process (callCommand)

import Yesod.Auth.Email
import qualified Yesod.Auth.Message as Msg

mailgunDomain :: Text
mailgunDomain = "mail.binchicken.one"

mailgunApiKey :: IO Text
mailgunApiKey = fromString <$> getEnv "BINCHICKEN_MAILGUN_API"

verEmailBody
  :: Text -- ^ the verification url
  -> Text -- ^ the full text of the email to be sendStatusJSON
verEmailBody verurl = "You are receiving this email because someone is either registering this email address at Binchicken or attempting to reset your password there. "
                        <> "You can confirm that this is you by clicking on this link: "
                        <> verurl
                        <> " Thanks! "
                        <> "If you did not request this email, please ignore it; you will not be registered, and if you are registered your password will not be changed."

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


data ForgotPasswordForm = ForgotPasswordForm { _forgotEmail :: Text }

binForgotPasswordHandler :: YesodAuthEmail master => AuthHandler master Html
binForgotPasswordHandler = do
    (widget, enctype) <- generateFormPost forgotPasswordForm
    toParent <- getRouteToParent
    authLayout $ do
        setTitleI Msg.PasswordResetTitle
        [whamlet|
            <p>Enter your registered email address below, and you'll get a password reset link sent to you.
            <form method=post action=@{toParent forgotPasswordR} enctype=#{enctype}>
                <div id="forgotPasswordForm">
                    ^{widget}
                    <button .btn .btn-success>_{Msg.SendPasswordResetEmail}
        |]
  where
    forgotPasswordForm extra = do
        (emailRes, emailView) <- mreq emailField emailSettings Nothing

        let forgotPasswordRes = ForgotPasswordForm <$> emailRes
        let widget = do
              [whamlet|
                  #{extra}
                  Email:
                  ^{fvInput emailView}
              |]
        return (forgotPasswordRes, widget)

    emailSettings =
        FieldSettings {
            fsLabel = SomeMessage Msg.ProvideIdentifier,
            fsTooltip = Nothing,
            fsId = Just "forgotPassword",
            fsName = Just "email",
            fsAttrs = [("autofocus", "")]
            }

data UserLoginForm = UserLoginForm { _loginEmail :: Text, _loginPassword :: Text }

binEmailLoginHandler
  :: YesodAuthEmail master
  => (Route Auth -> Route master)
  -> WidgetFor master ()
binEmailLoginHandler toParent = do
        (widget, enctype) <- generateFormPost loginForm

        [whamlet|
            <form method="post" action="@{toParent loginR}" enctype=#{enctype}>
                <div id="emailLoginForm">
                    ^{widget}
                    <div>
                        <button type=submit .btn .btn-success>
                            _{Msg.LoginViaEmail}
                        &nbsp;
                        <a href="@{toParent registerR}" .btn .btn-success>
                            _{Msg.RegisterLong}
            <div>
              <a href="@{toParent forgotPasswordR}">Forgot password?
        |]
  where
    loginForm extra = do

        emailMsg <- renderMessage' Msg.Email
        (emailRes, emailView) <- mreq emailField (emailSettings emailMsg) Nothing

        passwordMsg <- renderMessage' Msg.Password
        (passwordRes, passwordView) <- mreq passwordField (passwordSettings passwordMsg) Nothing

        let userRes = UserLoginForm <$> emailRes
                                    <*> passwordRes
        let widget = do
              [whamlet|
                  #{extra}
                  <div>
                      ^{fvInput emailView}
                  <div>
                      ^{fvInput passwordView}
              |]

        return (userRes, widget)
    emailSettings emailMsg = do
        FieldSettings {
            fsLabel = SomeMessage Msg.Email,
            fsTooltip = Nothing,
            fsId = Just "email",
            fsName = Just "email",
            fsAttrs = [("autofocus", ""), ("placeholder", emailMsg)]
        }
    passwordSettings passwordMsg =
         FieldSettings {
            fsLabel = SomeMessage Msg.Password,
            fsTooltip = Nothing,
            fsId = Just "password",
            fsName = Just "password",
            fsAttrs = [("placeholder", passwordMsg)]
        }
    renderMessage' msg = do
        langs <- languages
        master <- getYesod
        return $ renderAuthMessage master langs msg
