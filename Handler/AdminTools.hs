module Handler.AdminTools 
       ( getUserListR
       , getUserProfileR
       , postUserProfileR
       , postUserPasswordR
       , getUserEmailR
       , postUserEmailR
       , postCreateUserR
       , postKillUserR
       , getImportCsvR
       , postImportCsvR
       , getClientListR
       ) where

import Import
import qualified Data.ByteString.Char8 as BC
import Data.CSV.Conduit (defCSVSettings)
import Data.CSV.Conduit.Parser.ByteString (parseCSV)
import Data.Conduit (($$))
import Data.Conduit.List (consume)
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Owl.Helpers.Auth.HashDB (setPassword)
import Owl.Helpers.Widget
import Owl.Helpers.Form
import Owl.Helpers.Util
import Settings (userNumPerPage)

getUserListR :: Handler RepHtml
getUserListR = do
  (modalCreateUser, modalEditUser, modalKillUser) <- newIdent3
  (mq, mp) <- (,) <$> lookupGetParam "q" <*> lookupGetParam "p"
  let p = maybe 0 (read . T.unpack) mp
  us <- runDB $ selectList (like mq) [ Asc UserId
                                     , LimitTo userNumPerPage
                                     , OffsetBy (userNumPerPage * p)
                                     ]
  mmsg <- getMessage
  defaultLayout $ do
    setTitleI MsgMaintUser
    $(widgetFile "user-list")
  where
    like Nothing = []
    like (Just q) = [UserUsername `ilike` q]

getUserProfileR :: UserId -> Handler RepJson
getUserProfileR uid = do
  u <- runDB $ get404 uid
  jsonToRepJson $ object [ "username" .= userUsername u
                         , "familyname" .= userFamilyname u
                         , "givenname" .= userGivenname u
                         , "fullname" .= userFullname u
                         , "role" .= show (fromEnum (userRole u) + 1)
                         , "comment" .= fmap unTextarea (userComment u)
                         , "avatarUrl80" .= gravatarUrl 80 (userMd5hash' u)
                         ]

postUserProfileR :: UserId -> Handler ()
postUserProfileR uid = do
  ((r, _), _) <- runFormPost $ profileForm' Nothing
  case r of
    FormSuccess (fn, gn, role, cmt) -> do
      runDB $ update uid [UserFamilyname =. fn, UserGivenname =. gn, UserRole =. role, UserComment =. cmt]
      setMessageI MsgUpdateProfile
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailUpdateProfile
  redirect $ AdminTool UserListR

postUserPasswordR :: UserId -> Handler ()
postUserPasswordR uid = do
  u <- runDB $ get404 uid
  ((r, _), _) <- runFormPost $ passwordForm Nothing
  case r of
    FormSuccess newPass -> do
      runDB $ replace uid =<< setPassword newPass u
      setMessageI MsgPasswordUpdated
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToUpdatePassword
  redirect $ AdminTool UserListR

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
  ((r, _), _) <- runFormPost $ emailForm' Nothing
  case r of
    FormSuccess (memail, mverstatus, mverkey) -> do
      runDB $ update uid [UserEmail =. memail, UserVerstatus =. mverstatus, UserVerkey =. mverkey]
      setMessageI MsgUpdateEmailaddress
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToUpdateEmail
  redirect $ AdminTool UserListR

postCreateUserR :: Handler ()
postCreateUserR = do
  ((r, _), _) <- runFormPost $ newAccountForm Nothing
  case r of
    FormSuccess (uname, role, pass) -> do
      runDB $ do
        uid <- insert $ defUser { userUsername = uname, userRole = role }
        replace uid =<< setPassword pass =<< get404 uid
      setMessageI MsgCreateNewFace
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToCreateUser
  redirect $ AdminTool UserListR

postKillUserR :: UserId -> Handler ()
postKillUserR uid = do
  runDB $ delete uid
  setMessageI MsgUserKilled
  redirect $ AdminTool UserListR

getImportCsvR :: Handler RepHtml
getImportCsvR = do
  mmsg <- getMessage
  (w, e) <- generateFormPost $ fileForm Nothing
  defaultLayout $ do
    setTitleI MsgImportUser
    $(widgetFile "import-users-csv")

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
  redirect $ AdminTool ImportCsvR
  where
    importCSV :: [[BC.ByteString]] -> Handler ()
    importCSV = runDB . mapM_ importRow . fmap (fmap decodeUtf8) . filter ((>=5).length)
    importRow (uname:rawpass:email:fname:gname:_) = do
      uid <- maybe 
             (insert $ defUser { userUsername=uname
                               , userFamilyname=fname
                               , userGivenname=gname
                               , userEmail=Just email
                               })
             (return . entityKey)
             =<< getBy (UniqueUser uname)
      replace uid =<< setPassword rawpass =<< get404 uid

getClientListR :: Handler RepHtml
getClientListR = do
  mmsg <- getMessage
  modalEditClient <- newIdent
  let clients = [ ("a7362hd", "Kestrel", "aY/ay7w2hhuqwy9138")
                , ("97asdh2", "BISocie", "9wae/adisae9dcIOSJ")
                , ("8asASxp", "Owl",     "SI8weddUH.DHIDU-sd")
                ]::[(Text, Text, Text)]
  defaultLayout $ do
    setTitleI MsgMaintClient
    $(widgetFile "client-list")
