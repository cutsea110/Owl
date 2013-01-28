module Owl.Helpers.Widget where

import Import
import Prelude (head, tail)
import Data.Tuple.HT (fst3, snd3, thd3)
import Owl.Helpers.Form
import Owl.Helpers.Util (newIdent2, newIdent3, gravatarUrl)
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
  let (ue, memail, vs) = (entityVal u, userEmail ue, userVerstatus ue)
  (w, e) <- lift $ generateFormPost $ emailForm vs [("class", "span3"),("placeholder","you@example.com")] memail
  r <- lift getUrlRender
  $(widgetFile "email")

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
  (modalEditUser, modalKillUser) <- lift newIdent2
  us <- lift $ runDB $ selectList [] []
  $(widgetFile "user-list")

editUserWidget :: Widget
editUserWidget = do
  u <- lift requireAuth
  (menuProfile, menuPassword, menuEmail) <- lift newIdent3
  let passform = passwordConfirmForm (entityVal u) Nothing
  $(widgetFile "edit-user")

killUserWidget :: Widget
killUserWidget = do
  $(widgetFile "kill-user")

clientListWidget :: Widget
clientListWidget = do
  modalEditClient <- lift newIdent
  let clients = [ ("a7362hd", "Kestrel", "aY/ay7w2hhuqwy9138yihdu_lUSY26hauiehw7a87329yhiUHLUS")
                , ("97asdh2", "BISocie", "9wae/adisae9dcIOSJiidasiIOi42i472hDHjads8HIy98HDU7g9")
                , ("8asASxp", "Owl", "SI8weddUH.DHIDU-sdahsid/HDHUAIDdsuasdhuiasiad924422h")
                ]::[(Text, Text, Text)]
  $(widgetFile "client-list")

editClientWidget :: Widget
editClientWidget = do
  (clientId, clientName, clientSecret) <- lift newIdent3
  $(widgetFile "edit-client")
