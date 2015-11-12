module Handler.Config where

import Import

getConfigR :: Handler Html
getConfigR = do
  defaultLayout $ do
    setTitle "Config user setting"
    $(widgetFile "config-user-setting")
  