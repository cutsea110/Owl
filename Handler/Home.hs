{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = do
    defaultLayout $ do
        let accountId = $(widgetFile "account-id")
            password = $(widgetFile "password")
            email = $(widgetFile "email")
            profile = $(widgetFile "profile")
        setTitle "Home"
        $(widgetFile "homepage")
