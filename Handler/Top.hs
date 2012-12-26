module Handler.Top where

import Import
import Yesod.Auth

getTopR :: Handler RepHtml
getTopR = do
  mu <- maybeAuth
  defaultLayout $ do
    setTitle "Top"
    $(widgetFile "owl-top")
