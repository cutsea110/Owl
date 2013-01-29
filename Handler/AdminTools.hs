module Handler.AdminTools 
       ( getAdminToolsR
       , getUserProfileR
       , postUserProfileR
       , postUserPasswordR
       , getUserEmailR
       , postUserEmailR
       , postKillUserR
       ) where

import Import
import Owl.Helpers.Auth.HashDB (setPassword)
import Owl.Helpers.Widget
import Owl.Helpers.Form
import Owl.Helpers.Util (newIdent3, gravatarUrl)

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
                         , "fullname" .= userFullname u
                         , "comment" .= fmap unTextarea (userComment u)
                         , "avatarUrl80" .= gravatarUrl 80 (userMd5hash' u)
                         ]

postUserProfileR :: UserId -> Handler ()
postUserProfileR uid = do
  ((r, _), _) <- runFormPost $ profileForm Nothing
  case r of
    FormSuccess (fn, gn, cmt) -> do
      runDB $ update uid [UserFamilyname =. fn, UserGivenname =. gn, UserComment =. cmt]
      setMessageI MsgUpdateProfile
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailUpdateProfile
  redirect $ AdminTool AdminToolsR -- FIXME!

postUserPasswordR :: UserId -> Handler ()
postUserPasswordR uid = do
  u <- runDB $ get404 uid
  ((r, _), _) <- runFormPost $ passwordConfirmForm u Nothing
  case r of
    FormSuccess newPass -> do
      runDB $ replace uid =<< setPassword newPass u
      setMessageI MsgPasswordUpdated
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToUpdatePassword
  redirect $ AdminTool AdminToolsR

getUserEmailR :: UserId -> Handler RepJson
getUserEmailR uid = do
  u <- runDB $ get404 uid
  jsonToRepJson $ object [ "email" .= userEmail u
                         , "verstatus" .= fmap ((+1).fromEnum) (userVerstatus u)
                         , "verstatus_str" .= fmap show (userVerstatus u)
                         , "verkey" .= userVerkey u
                         ]

postUserEmailR :: UserId -> Handler ()
postUserEmailR uid = do
  ((r, _), _) <- runFormPost $ userEmailForm [] Nothing
  case r of
    FormSuccess (memail, mverstatus, mverkey) -> do
      runDB $ update uid [UserEmail =. memail, UserVerstatus =. mverstatus, UserVerkey =. mverkey]
      setMessageI MsgUpdateEmailaddress
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToUpdateEmail
  redirect $ AdminTool AdminToolsR

postKillUserR :: UserId -> Handler ()
postKillUserR uid = do
  runDB $ delete uid
  redirect $ AdminTool AdminToolsR
