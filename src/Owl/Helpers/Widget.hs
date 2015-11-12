module Owl.Helpers.Widget where

import Import
import Owl.Helpers.Form
import Owl.Helpers.Util (newIdent3)
import Text.Julius (rawJS)

passwordWidget :: Form Text -> Route App -> Widget
passwordWidget form toPost = do
  _u <- handlerToWidget requireAuth
  r <- handlerToWidget getUrlRender
  (w, e) <- handlerToWidget $ generateFormPost form
  $(widgetFile "password")

emailWidget :: Route App -> Widget
emailWidget toPost = do
  u <- handlerToWidget requireAuth
  r <- handlerToWidget getUrlRender
  (w, e) <- handlerToWidget $ generateFormPost $ emailForm $ userEmail $ entityVal u
  $(widgetFile "email")

userEmailWidget :: Maybe User -> Route App -> Widget
userEmailWidget mu toPost = do
  let (memail, mverstatus, mverkey) = (join $ userEmail <$> mu, join $ userVerstatus <$> mu, join $ userVerkey <$> mu)
  (w, e) <- handlerToWidget $ generateFormPost $ emailForm' $ Just (memail, mverstatus, mverkey)
  r <- handlerToWidget getUrlRender
  $(widgetFile "user-email")

verifyWidget :: Maybe Text -> Route App -> [(Text, Text)] -> Widget
verifyWidget mv toPost params = do
  r <- handlerToWidget getUrlRenderParams
  (w, e) <- handlerToWidget $ generateFormPost $ verifyForm mv
  $(widgetFile "verify")

profileWidget :: Route App -> Widget
profileWidget toPost = do
  (u, r) <- handlerToWidget $ (,) <$> requireAuth <*> getUrlRender
  let mv = Just $ (,,) <$> userFamilyname <*> userGivenname <*> userComment $ entityVal u
  (w, e) <- handlerToWidget $ generateFormPost $ profileForm mv
  $(widgetFile "profile")

profileWidget' :: Route App -> Widget
profileWidget' toPost = do
  (u, r) <- handlerToWidget $ (,) <$> requireAuth <*> getUrlRender
  let mv = Just $ (,,,) <$> userFamilyname <*> userGivenname <*> userRole <*> userComment $ entityVal u
  (w, e) <- handlerToWidget $ generateFormPost $ profileForm' mv
  $(widgetFile "profile")

createUserWidget :: Maybe (Text, Role, Text, Text) -> Route App -> Widget
createUserWidget mv toPost = do
  (w, e) <- handlerToWidget $ generateFormPost $ newAccountForm mv
  r <- handlerToWidget $ getUrlRender
  $(widgetFile "create-user")

editUserWidget :: Widget
editUserWidget = do
  _u <- handlerToWidget requireAuth
  (menuProfile, menuPassword, menuEmail) <- handlerToWidget newIdent3
  let passform = passwordForm Nothing
  $(widgetFile "edit-user")

killUserWidget :: Widget
killUserWidget = do
  $(widgetFile "kill-user")

editClientWidget :: Widget
editClientWidget = do
  (cId, cName, cSecret) <- handlerToWidget newIdent3
  $(widgetFile "edit-client")
