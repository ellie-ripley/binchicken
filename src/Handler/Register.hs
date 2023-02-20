{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Register where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

-- Define our data that will be used for creating the form.
data RegistrationForm = RegistrationForm
    { newUserEmail :: Text
    , newUserPassword :: Text
    , newUserPasswordConfirm :: Text
    }
    deriving (Show)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRegisterR :: Handler Html
getRegisterR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let successMessage = Nothing :: Maybe Text

    request <- getRequest
    defaultLayout $ do
        setTitle "Registration"
        $(widgetFile "register")

data SubmissionStatus =
    Success (Key User)
  | Failure
  | PwMismatch
  deriving (Show)

displaySubmissionStatus :: SubmissionStatus -> Text
displaySubmissionStatus = \case
  Success _  -> "Success!"
  Failure    -> "Failed to register; probably the email is taken already"
  PwMismatch -> "Passwords did not match"

postRegisterR :: Handler Html
postRegisterR = do
    (emal, pw, pwc) <- runInputPost
                        ((,,) <$> ireq textField "email"
                              <*> ireq textField "password"
                              <*> ireq textField "confirm")

    subStat <- if pw == pwc
                  then do
                        attempt <- runDB . insertUnique $ (User emal pw)
                        case attempt of
                          Just ky -> return (Success ky)
                          Nothing -> return Failure
               else return PwMismatch
    let successMessage = Just $ displaySubmissionStatus subStat

    request <- getRequest
    defaultLayout $ do
        setTitle "Registration"
        $(widgetFile "register")

sampleForm :: Form RegistrationForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ RegistrationForm
    <$> areq textField "Email: " Nothing
    <*> areq textField "Password: " Nothing
    <*> areq textField "Confirm password: " Nothing

getAllUsers :: DB [Entity User]
getAllUsers = selectList [] [Asc UserEmail]
