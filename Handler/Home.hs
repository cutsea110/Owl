{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Prelude (head, tail)
import Yesod.Auth

getHomeR :: Handler RepHtml
getHomeR = do
  u <- requireAuth
  defaultLayout $ do
    modal1 <- lift newIdent
    modal2 <- lift newIdent
    let photos = [ (img_avatar_avatar_jpg, "Photo 1"::Text)
                 , (img_avatar_avatar2_jpg, "Photo 2")
                 , (img_avatar_avatar3_jpg, "Photo 3")
                 , (img_avatar_avatar4_jpg, "Photo 4")
                 , (img_avatar_avatar5_jpg, "Photo 5")
                 , (img_avatar_avatar6_jpg, "Photo 6")
                 , (img_avatar_avatar7_jpg, "Photo 7")
                 , (img_avatar_avatar8_jpg, "Photo 8")
                 , (img_avatar_avatar9_jpg, "Photo 9")
                 ]
        avatar = head photos
    let accountId = $(widgetFile "account-id")
        password = $(widgetFile "password")
        email = $(widgetFile "email")
        profile = $(widgetFile "profile")
        changeAvatar = $(widgetFile "change-avatar")
        uploadPhotos = $(widgetFile "upload-photos")
    setTitle "Home"
    $(widgetFile "homepage")
