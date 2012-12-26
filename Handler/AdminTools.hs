module Handler.AdminTools where

import Import

getAdminToolsR :: Handler RepHtml
getAdminToolsR = do
  defaultLayout $ do
    setTitle "Administrator's Tools"
    $(widgetFile "admin-tools")
