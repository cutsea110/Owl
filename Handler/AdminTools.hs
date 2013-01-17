module Handler.AdminTools 
       ( getAdminToolsR
       , getUserProfileR
       , postUserProfileR
       ) where

import Import
import Owl.Helpers.Widget
import Owl.Helpers.Form
import Owl.Helpers.Util (newIdent3)

getAdminToolsR :: Handler RepHtml
getAdminToolsR = do
  (menuMaintUser, menuImportUsers, menuMaintClient) <- newIdent3
  tabIs <- fmap (maybe ("maint-user"==) (==)) $ lookupGetParam "tab"
  mmsg <- getMessage
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

postUserProfileR :: UserId -> Handler ()
postUserProfileR uid = do
  ((r,_), _) <- runFormPost $ profileForm Nothing
  case r of
    FormSuccess (fn, gn, cmt) -> do
      runDB $ update uid [UserFamilyname =. fn, UserGivenname =. gn, UserComment =. cmt]
      setMessageI MsgUpdateProfile
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailUpdateProfile
  redirect $ AdminTool AdminToolsR -- FIXME!
