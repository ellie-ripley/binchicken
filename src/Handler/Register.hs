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
    let submission = Nothing :: Maybe RegistrationForm
        newUser = NoAttempt

    defaultLayout $ do
        setTitle "Registration"
        $(widgetFile "register")

data SubmissionStatus =
    Success (Key User)
  | Failure
  | NoAttempt
  deriving (Show)

postRegisterR :: Handler Html
postRegisterR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let submission = case result of
            FormSuccess rform -> Just rform
            _ -> Nothing

    newUser <- case submission of
                 Just rform -> do
                   attempt <- runDB . insertUnique $ (User (newUserEmail rform)
                                                           (newUserPassword rform))
                   case attempt of
                     Just ky -> return (Success ky)
                     Nothing -> return Failure
                 Nothing -> return NoAttempt

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
