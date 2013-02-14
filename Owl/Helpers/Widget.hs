module Owl.Helpers.Widget where

import Import
import Prelude (head, tail)
import Control.Monad (join)
import Data.Tuple.HT (fst3, snd3, thd3)
import Owl.Helpers.Form
import Owl.Helpers.Util (newIdent3, gravatarUrl)
import Text.Julius (rawJS)
import Yesod.Auth (requireAuth)
import Yesod.Routes.Class (Route)

passwordWidget :: Form Text -> Route App -> Widget
passwordWidget form toPost = do
  u <- lift requireAuth
  (w, e) <- lift $ generateFormPost form
  r <- lift getUrlRender
  $(widgetFile "password")

emailWidget :: Route App -> Widget
emailWidget toPost = do
  u <- lift requireAuth
  let (ue, memail, mverstatus) = (entityVal u, userEmail ue, userVerstatus ue)
  (w, e) <- lift $ generateFormPost $ emailForm memail
  r <- lift getUrlRender
  $(widgetFile "email")

userEmailWidget :: Maybe User -> Route App -> Widget
userEmailWidget mu toPost = do
  let (memail, mverstatus, mverkey) = (join $ userEmail <$> mu, join $ userVerstatus <$> mu, join $ userVerkey <$> mu)
  (w, e) <- lift $ generateFormPost $ userEmailForm [("class", "span3"),("placeholder","you@example.com")] $ Just (memail, mverstatus, mverkey)
  r <- lift getUrlRender
  $(widgetFile "user-email")

verifyWidget :: Maybe Text -> Route App -> [(Text, Text)] -> Widget
verifyWidget mv toPost params = do
  (w, e) <- lift $ generateFormPost $ verifyForm mv
  r <- lift getUrlRenderParams
  $(widgetFile "verify")

importCsvWidget :: Widget
importCsvWidget = do
  (w, e) <- lift $ generateFormPost $ fileForm Nothing
  $(widgetFile "import-users-csv")

profileWidget :: Route App -> Widget
profileWidget toPost = do
  u <- fmap entityVal $ lift requireAuth
  let mv = Just $ (,,) <$> userFamilyname <*> userGivenname <*> userComment $ u
  (w, e) <- lift $ generateFormPost $ profileForm mv
  r <- lift getUrlRender
  $(widgetFile "profile")

userListWidget :: Widget
userListWidget = do
  (modalCreateUser, modalEditUser, modalKillUser) <- lift newIdent3
  us <- lift $ runDB $ selectList [] [Asc UserId]
  $(widgetFile "user-list")

createUserWidget :: Maybe (Text, Text, Text) -> Route App -> Widget
createUserWidget mv toPost = do
  (w, e) <- lift $ generateFormPost $ accountPasswordForm mv
  r <- lift getUrlRender
  $(widgetFile "create-user")

editUserWidget :: Widget
editUserWidget = do
  u <- lift requireAuth
  (menuProfile, menuPassword, menuEmail) <- lift newIdent3
  let passform = passwordConfirmForm Nothing
  $(widgetFile "edit-user")

killUserWidget :: Widget
killUserWidget = do
  $(widgetFile "kill-user")

clientListWidget :: Widget
clientListWidget = do
  modalEditClient <- lift newIdent
  let clients = [ ("a7362hd", "Kestrel", "aY/ay7w2hhuqwy9138")
                , ("97asdh2", "BISocie", "9wae/adisae9dcIOSJ")
                , ("8asASxp", "Owl",     "SI8weddUH.DHIDU-sd")
                ]::[(Text, Text, Text)]
  $(widgetFile "client-list")

editClientWidget :: Widget
editClientWidget = do
  (clientId, clientName, clientSecret) <- lift newIdent3
  $(widgetFile "edit-client")
