module Handler.AdminTools 
       ( getAdminToolsR
       ) where

import Import
import Owl.Helpers.Widget
import Owl.Helpers.Util (newIdent3)

getAdminToolsR :: Handler RepHtml
getAdminToolsR = do
  (menu1, menu2, menu3) <- newIdent3
  tabIs <- fmap (maybe ("maint-user"==) (==)) $ lookupGetParam "tab"
  defaultLayout $ do
    setTitle "Administrator's Tools"
    $(widgetFile "admin-tools")
