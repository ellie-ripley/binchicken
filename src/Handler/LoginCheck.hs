{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.LoginCheck where



import Import ( Widget
              , maybeAuthId
              , whamlet
              )

loginNotifyW :: Widget
loginNotifyW = do
  maid <- maybeAuthId
  case maid of
    Just _  -> mempty
    Nothing -> [whamlet|<div .alert.alert-info>
                             <p>You're not logged in. Exercises work as usual, but progress is not tracked.
               |]
