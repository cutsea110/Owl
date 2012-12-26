module Handler.Help where

import Import

getHelpR :: Handler RepHtml
getHelpR = do
  defaultLayout $ do
    setTitle "Help"
    $(widgetFile "help")
