module Owl.Helpers.Widget where

import Import
import Prelude (head, tail)
import Control.Monad (join)
import Owl.Helpers.Form
import Owl.Helpers.Util (newIdent3)
import Text.Julius (rawJS)
import Yesod.Auth (requireAuth)
import Yesod.Routes.Class (Route)

passwordWidget :: Form Text -> Route App -> Widget
passwordWidget form toPost = do
  u <- lift requireAuth
  r <- lift getUrlRender
  (w, e) <- lift $ generateFormPost form
  $(widgetFile "password")

emailWidget :: Route App -> Widget
emailWidget toPost = do
  u <- lift requireAuth
  r <- lift getUrlRender
  (w, e) <- lift $ generateFormPost $ emailForm $ userEmail $ entityVal u
  $(widgetFile "email")

userEmailWidget :: Maybe User -> Route App -> Widget
userEmailWidget mu toPost = do
  let (memail, mverstatus, mverkey) = (join $ userEmail <$> mu, join $ userVerstatus <$> mu, join $ userVerkey <$> mu)
  (w, e) <- lift $ generateFormPost $ emailForm' $ Just (memail, mverstatus, mverkey)
  r <- lift getUrlRender
  $(widgetFile "user-email")

verifyWidget :: Maybe Text -> Route App -> [(Text, Text)] -> Widget
verifyWidget mv toPost params = do
  r <- lift getUrlRenderParams
  (w, e) <- lift $ generateFormPost $ verifyForm mv
  $(widgetFile "verify")

profileWidget :: Route App -> Widget
profileWidget toPost = do
  (u, r) <- lift $ (,) <$> requireAuth <*> getUrlRender
  let mv = Just $ (,,) <$> userFamilyname <*> userGivenname <*> userComment $ entityVal u
  (w, e) <- lift $ generateFormPost $ profileForm mv
  $(widgetFile "profile")

profileWidget' :: Route App -> Widget
profileWidget' toPost = do
  (u, r) <- lift $ (,) <$> requireAuth <*> getUrlRender
  let mv = Just $ (,,,) <$> userFamilyname <*> userGivenname <*> userRole <*> userComment $ entityVal u
  (w, e) <- lift $ generateFormPost $ profileForm' mv
  $(widgetFile "profile")

createUserWidget :: Maybe (Text, Role, Text, Text) -> Route App -> Widget
createUserWidget mv toPost = do
  (w, e) <- lift $ generateFormPost $ newAccountForm mv
  r <- lift $ getUrlRender
  $(widgetFile "create-user")

editUserWidget :: Widget
editUserWidget = do
  u <- lift requireAuth
  (menuProfile, menuPassword, menuEmail) <- lift newIdent3
  let passform = passwordForm Nothing
  $(widgetFile "edit-user")

killUserWidget :: Widget
killUserWidget = do
  $(widgetFile "kill-user")

editClientWidget :: Widget
editClientWidget = do
  (cId, cName, cSecret) <- lift newIdent3
  $(widgetFile "edit-client")
