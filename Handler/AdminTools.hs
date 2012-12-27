module Handler.AdminTools where

import Import

getAdminToolsR :: Handler RepHtml
getAdminToolsR = do
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
  defaultLayout $ do
    let userCrud = $(widgetFile "user-list")
    setTitle "Administrator's Tools"
    $(widgetFile "admin-tools")
