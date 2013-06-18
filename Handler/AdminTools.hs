{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
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
import Data.Time (getCurrentTime)
import Owl.Helpers.Auth.HashDB (setPassword)
import Owl.Helpers.Widget
import Owl.Helpers.Form
import Owl.Helpers.Util

getUserListR :: Handler Html
getUserListR = do
  (modalCreateUser, modalEditUser, modalKillUser) <- newIdent3
  (mq, mp) <- (,) <$> lookupGetParam "q" <*> lookupGetParam "p"
  let (p, qf) = (maybe 0 (read . T.unpack) mp,
                 maybeToList $ fmap (ilike UserUsername) mq)
  (c, us) <- runDB $ (,)
        <$> count qf
        <*> selectList qf [Asc UserId, LimitTo userNumPerPage, OffsetBy (userNumPerPage * p)]
  let (q, pages) =
        (maybeToList $ fmap ("q",) mq,
         pagenate fillGapWidth pagenateWidth userNumPerPage (AdminTool UserListR, q) c p)
  mmsg <- getMessage
  defaultLayout $ do
    setTitleI MsgMaintUser
    $(widgetFile "user-list")

getUserProfileR :: UserId -> Handler Value
getUserProfileR uid = do
  u <- runDB $ get404 uid
  returnJson $ object [ "username" .= userUsername u
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
      now <- liftIO getCurrentTime
      runDB $ update uid [ UserFamilyname =. fn
                         , UserGivenname =. gn
                         , UserRole =. role
                         , UserComment =. cmt
                         , UserUpdated =. now
                         ]
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
      now <- liftIO getCurrentTime
      runDB $ replace uid =<< setPassword newPass u { userUpdated = now }
      setMessageI MsgPasswordUpdated
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToUpdatePassword
  redirect $ AdminTool UserListR

getUserEmailR :: UserId -> Handler Value
getUserEmailR uid = do
  u <- runDB $ get404 uid
  returnJson $ object [ "email" .= userEmail u
                      , "verstatus" .= fmap ((+1).fromEnum) (userVerstatus u)
                      , "verstatus_str" .= fmap show (userVerstatus u)
                      , "verkey" .= userVerkey u
                      ]

postUserEmailR :: UserId -> Handler ()
postUserEmailR uid = do
  ((r, _), _) <- runFormPost $ emailForm' Nothing
  case r of
    FormSuccess (memail, mverstatus, mverkey) -> do
      now <- liftIO getCurrentTime
      runDB $ update uid [ UserEmail =. memail
                         , UserVerstatus =. mverstatus
                         , UserVerkey =. mverkey
                         , UserUpdated =. now
                         ]
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
        usr <- liftIO newUser
        uid <- insert $ usr { userUsername = uname, userRole = role }
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

getImportCsvR :: Handler Html
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
      lbs <- BC.unlines <$> (fileSource fi $$ consume)
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
      usr <- liftIO newUser
      uid <- maybe
             (insert $ usr { userUsername=uname
                           , userFamilyname=fname
                           , userGivenname=gname
                           , userEmail=Just email
                           })
             (return . entityKey)
             =<< getBy (UniqueUser uname)
      replace uid =<< setPassword rawpass =<< get404 uid

getClientListR :: Handler Html
getClientListR = do
  mmsg <- getMessage
  modalEditClient <- newIdent
  let clients = clientPublicKeys
  defaultLayout $ do
    setTitleI MsgMaintClient
    $(widgetFile "client-list")
