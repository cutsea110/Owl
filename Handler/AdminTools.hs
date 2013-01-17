module Handler.AdminTools 
       ( getAdminToolsR
       , getUserProfileR
       ) where

import Import
import Owl.Helpers.Widget
import Owl.Helpers.Util (newIdent3)

getAdminToolsR :: Handler RepHtml
getAdminToolsR = do
  (menuMaintUser, menuImportUsers, menuMaintClient) <- newIdent3
  tabIs <- fmap (maybe ("maint-user"==) (==)) $ lookupGetParam "tab"
  defaultLayout $ do
    setTitle "Administrator's Tools"
    $(widgetFile "admin-tools")

getUserProfileR :: UserId -> Handler RepJson
getUserProfileR uid = do
  u <- runDB $ get404 uid
  jsonToRepJson $ object [ "username" .= userUsername u
                         , "familyname" .= userFamilyname u
                         , "givenname" .= userGivenname u
                         , "comment" .= fmap unTextarea (userComment u)
                         ]
