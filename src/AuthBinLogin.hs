{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module AuthBinLogin (YesodAuthBinLogin(..), binLogin) where

import           Data.Text        (Text)
import Yesod.Auth
    ( Route(LoginR, PluginR),
      loginErrorMessageI,
      setCredsRedirect,
      AuthHandler,
      AuthPlugin(AuthPlugin),
      AuthRoute,
      Creds(Creds),
      YesodAuth )
import Yesod.Core
    ( defaultCsrfParamName,
      getRequest,
      notFound,
      sendResponse,
      whamlet,
      TypedContent,
      YesodRequest(reqToken),
      MonadHandler,
      HandlerSite)
import           Yesod.Form       (ireq, runInputPost, textField)
import qualified Yesod.Auth.Message as Msg

binLoginR :: AuthRoute
binLoginR = PluginR "binLogin" ["login"]

class (YesodAuth site) => YesodAuthBinLogin site where
  -- | check whether login info works (username, surname, id)
  doesUserExist :: (MonadHandler m, HandlerSite m ~ site)
                => Text -> Text -> Text -> m Bool

binLogin :: YesodAuthBinLogin m => AuthPlugin m
binLogin =
    AuthPlugin "binLogin" dispatch login
  where
    dispatch :: YesodAuthBinLogin m => Text -> [Text] -> AuthHandler m TypedContent
    dispatch "POST" ["login"] = postBinLoginR >>= sendResponse
    dispatch "GET" ["login"] = sendResponse ("Hi!" :: String)
    dispatch _ _ = notFound
    login authToMaster = do
      request <- getRequest
      [whamlet|
          <form method="post" action="@{authToMaster binLoginR}">
            $maybe t <- reqToken request
              <input type=hidden name=#{defaultCsrfParamName} value=#{t}>
              <table>
                <tr>
                  <th>Username
                  <td>
                    <input type="text" name="ident" required>
                <tr>
                  <th>Surname
                  <td>
                    <input type="text" name="surname" required>
                <tr>
                  <th>Id number
                  <td>
                    <input type="text" name="idnumber" required>
                <tr>
                  <td colspan="2">
                    <button type="submit" .btn .btn-success>_{Msg.LoginTitle}
      |]

postBinLoginR :: YesodAuthBinLogin site => AuthHandler site TypedContent
postBinLoginR = do
  (iden, surn, idnum) <- runInputPost
        ((,,) <$> ireq textField "ident"
              <*> ireq textField "surname"
              <*> ireq textField "idnumber")
  isValid <- doesUserExist iden surn idnum
  if isValid
  then setCredsRedirect (Creds "binLogin" iden [])
  else loginErrorMessageI LoginR (Msg.IdentifierNotFound iden)
