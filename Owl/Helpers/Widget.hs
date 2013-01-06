module Owl.Helpers.Widget where

import Import
import Prelude (head, tail)
import Owl.Helpers.Form
import Owl.Helpers.Util (newIdent2, newIdent3, newIdent4)
import Yesod.Routes.Class (Route)

accountWidget :: Route App -> Widget
accountWidget toPost = do
  (w, e) <- lift $ generateFormPost $ accountForm Nothing
  r <- lift getUrlRender
  $(widgetFile "account-id")

passwordWidget :: Route App -> Widget
passwordWidget toPost = do
  (w, e) <- lift $ generateFormPost $ passwordForm Nothing
  r <- lift getUrlRender
  $(widgetFile "password")

emailWidget :: Route App -> Widget
emailWidget toPost = do
  (w, e) <- lift $ generateFormPost $ emailForm [("class", "span3"),("placeholder","cutsea110@gmail.com")] Nothing
  r <- lift getUrlRender
  $(widgetFile "email")

changeAvatarWidget :: Widget
changeAvatarWidget = do
  avatarCarousel <- lift newIdent
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
  $(widgetFile "change-avatar")

uploadPhotoWidget :: Widget
uploadPhotoWidget = do
  (w, e) <- lift $ generateFormPost $ fileForm Nothing
  $(widgetFile "upload-photos")

importCsvWidget :: Widget
importCsvWidget = do
  (w, e) <- lift $ generateFormPost $ fileForm Nothing
  $(widgetFile "import-users-csv")

editProfileWidget :: Route App -> Widget
editProfileWidget toPost = do
  (w, e) <- lift $ generateFormPost $ profileForm Nothing
  r <- lift getUrlRender
  $(widgetFile "edit-profile")

profileWidget :: Widget
profileWidget= do
  (modalChangeAvatar, modalUploadPhotos, modalEditComment) <- lift newIdent3
  $(widgetFile "profile")

userListWidget :: Widget
userListWidget = do
  (modalEditUser, modalKillUser) <- lift newIdent2
  let photos = [ (img_avatar_avatar_jpg, "User 1"::Text)
               , (img_avatar_avatar2_jpg, "User 2")
               , (img_avatar_avatar3_jpg, "User 3")
               , (img_avatar_avatar4_jpg, "User 4")
               , (img_avatar_avatar5_jpg, "User 5")
               , (img_avatar_avatar6_jpg, "User 6")
               , (img_avatar_avatar7_jpg, "User 7")
               , (img_avatar_avatar8_jpg, "User 8")
               , (img_avatar_avatar9_jpg, "User 9")
               ]
  $(widgetFile "user-list")
  
editUserWidget :: Widget
editUserWidget = do
  (menuAccount, menuPassword, menuEmail, menuProfile) <- lift newIdent4
  $(widgetFile "edit-user")

killUserWidget :: Widget
killUserWidget = do
  $(widgetFile "kill-user")

clientListWidget :: Widget
clientListWidget = do
  modal1 <- lift newIdent
  let editclient = $(widgetFile "edit-client")
  $(widgetFile "client-list")
