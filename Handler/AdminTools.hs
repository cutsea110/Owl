module Handler.AdminTools 
       ( getAdminToolsR
       , getUserProfileR
       , postUserProfileR
       , postUserPasswordR
       , getUserEmailR
       , postUserEmailR
       , postCreateUserR
       , postKillUserR
       , postImportCsvR
       ) where

import Import
import qualified Data.ByteString.Char8 as BC
import Data.CSV.Conduit (defCSVSettings)
import Data.CSV.Conduit.Parser.ByteString (parseCSV)
import Data.Conduit (($$))
import Data.Conduit.List (consume)
import Data.Text.Encoding (decodeUtf8)
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
  redirect (AdminTool AdminToolsR, [("tab", "maint-user")])

postUserPasswordR :: UserId -> Handler ()
postUserPasswordR uid = do
  u <- runDB $ get404 uid
  ((r, _), _) <- runFormPost $ passwordConfirmForm Nothing
  case r of
    FormSuccess newPass -> do
      runDB $ replace uid =<< setPassword newPass u
      setMessageI MsgPasswordUpdated
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToUpdatePassword
  redirect (AdminTool AdminToolsR, [("tab", "maint-user")])

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
  ((r, _), _) <- runFormPost $ userEmailForm Nothing
  case r of
    FormSuccess (memail, mverstatus, mverkey) -> do
      runDB $ update uid [UserEmail =. memail, UserVerstatus =. mverstatus, UserVerkey =. mverkey]
      setMessageI MsgUpdateEmailaddress
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToUpdateEmail
  redirect (AdminTool AdminToolsR, [("tab", "maint-user")])

postCreateUserR :: Handler ()
postCreateUserR = do
  ((r, _), _) <- runFormPost $ accountPasswordForm Nothing
  case r of
    FormSuccess (uname, pass) -> do
      runDB $ do
        uid <- insert $ User { userUsername= uname 
                             , userPassword="" 
                             , userSalt="" 
                             , userRole=None 
                             , userFamilyname="" 
                             , userGivenname="" 
                             , userComment=Nothing 
                             , userEmail=Nothing 
                             , userVerkey=Nothing 
                             , userVerstatus=Nothing 
                             , userMd5hash=Nothing
                             }
        replace uid =<< setPassword pass =<< get404 uid
      setMessageI MsgCreateNewFace
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToCreateUser
  redirect (AdminTool AdminToolsR, [("tab", "maint-user")])

postKillUserR :: UserId -> Handler ()
postKillUserR uid = do
  runDB $ delete uid
  redirect (AdminTool AdminToolsR, [("tab", "maint-user")])

postImportCsvR :: Handler ()
postImportCsvR = do
  ((r, _), _) <- runFormPost $ fileForm Nothing
  case r of
    FormSuccess fi  -> do
      lbs <- lift $ BC.unlines <$> (fileSource fi $$ consume)
      either 
        (setMessage.toHtml) 
        (\csv -> importCSV csv >> setMessageI MsgSuccessImportUsers) 
        $ parseCSV defCSVSettings lbs
    _ -> setMessageI MsgFailToImportUsers
  redirect (AdminTool AdminToolsR, [("tab", "import-users")])
  where
    importCSV :: [[BC.ByteString]] -> Handler ()
    importCSV = runDB . mapM_ importRow . fmap (fmap decodeUtf8) . filter ((>=5).length)
    importRow (uname:rawpass:email:fname:gname:_) = do
      uid <- maybe 
             (insert $ User { userUsername=uname
                            , userPassword=""
                            , userSalt=""
                            , userRole=None
                            , userFamilyname=fname
                            , userGivenname=gname
                            , userComment=Nothing
                            , userEmail=Just email
                            , userVerkey=Nothing
                            , userVerstatus=Nothing
                            , userMd5hash=Nothing
                            })
              (return . entityKey)
             =<< getBy (UniqueUser uname)
      replace uid =<< setPassword rawpass =<< get404 uid
