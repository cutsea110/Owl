module Owl.Helpers.Widget where

import Import
import Prelude (head, tail)
import Data.Tuple.HT (fst3, snd3, thd3)
import Owl.Helpers.Form
import Owl.Helpers.Util (newIdent2, newIdent3)
import Text.Julius (rawJS)
import Yesod.Auth (requireAuth)
import Yesod.Routes.Class (Route)

passwordWidget :: Route App -> Widget
passwordWidget toPost = do
  u <- lift requireAuth
  (w, e) <- lift $ generateFormPost $ passwordForm (entityVal u) Nothing
  r <- lift getUrlRender
  $(widgetFile "password")

emailWidget :: Maybe VerStatus -> Route App -> Widget
emailWidget vs toPost = do
  memail <- fmap (userEmail.entityVal) $ lift requireAuth
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
  accountId <- lift newIdent
  u <- fmap entityVal $ lift requireAuth
  let mv = Just $ (,,) <$> userFamilyname <*> userGivenname <*> userComment $ u
  (w, e) <- lift $ generateFormPost $ profileForm mv
  r <- lift getUrlRender
  $(widgetFile "profile")

userListWidget :: Widget
userListWidget = do
  (modalEditUser, modalKillUser) <- lift newIdent2
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
  $(widgetFile "user-list")

editUserWidget :: Widget
editUserWidget = do
  u <- lift requireAuth
  (menuProfile, menuPassword, menuEmail) <- lift newIdent3
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
