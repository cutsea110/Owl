module Handler.AdminTools 
       ( getAdminToolsR
       ) where

import Import
import Data.Maybe (isJust)
import Text.Julius (rawJS)
import Owl.Helpers.Form (fileForm)

getAdminToolsR :: Handler RepHtml
getAdminToolsR = do
  (modal1, modal2, modal3) <- (,,) <$> newIdent <*> newIdent <*> newIdent
  (menu1, menu2, menu3) <- (,,) <$> newIdent <*> newIdent <*> newIdent
  (w, e) <- generateFormPost $ fileForm Nothing
  tabIs <- fmap (maybe ("maint-user"==) (==)) $ lookupGetParam "tab"
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
  defaultLayout $ do
    let userlist = $(widgetFile "user-list")
        edituser = $(widgetFile "edit-user")
        killuser = $(widgetFile "kill-user")
        importcsv = $(widgetFile "import-users-csv")
        clientlist = $(widgetFile "client-list")
        editclient = $(widgetFile "edit-client")
    setTitle "Administrator's Tools"
    $(widgetFile "admin-tools")
