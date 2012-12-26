module Handler.Config where

import Import

getConfigR :: Handler RepHtml
getConfigR = do
  defaultLayout $ do
    setTitle "Config user setting"
    $(widgetFile "config-user-setting")
  