{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth

getHomeR :: Handler RepHtml
getHomeR = do
  u <- requireAuth
  defaultLayout $ do
    let accountId = $(widgetFile "account-id")
        password = $(widgetFile "password")
        email = $(widgetFile "email")
        profile = $(widgetFile "profile")
    setTitle "Home"
    $(widgetFile "homepage")
