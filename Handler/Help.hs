module Handler.Help where

import Import

getHelpR :: Handler RepHtml
getHelpR = do
  defaultLayout $ do
    let passreset = $(widgetFile "password-reset")
        usage = $(widgetFile "usage")
    setTitle "Help"
    $(widgetFile "help")
