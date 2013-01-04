{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home 
       ( getHomeR
       , postAccountIdR
       , postPasswordR
       , postEmailR
       ) where

import Import
import Prelude (head, tail)
import Yesod.Auth
import qualified Data.Text as T
import Owl.Helpers.Form

getHomeR :: Handler RepHtml
getHomeR = do
  u <- requireAuth
  (menu1, menu2, menu3, menu4) <- (,,,) <$> newIdent <*> newIdent <*> newIdent <*> newIdent
  tabIs <- fmap (maybe ("account-id"==) (==)) $ lookupGetParam "tab"
  mmsg <- getMessage
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")

accountWidget :: Widget
accountWidget = do
  (w, e) <- lift $ generateFormPost $ accountForm Nothing
  $(widgetFile "account-id")
  
passwordWidget :: Widget
passwordWidget = do
  (w, e) <- lift $ generateFormPost $ passwordForm Nothing
  $(widgetFile "password")

emailWidget :: Widget
emailWidget = do
  (w, e) <- lift $ generateFormPost $ emailForm [("class", "span3"),("placeholder","cutsea110@gmail.com")] Nothing
  $(widgetFile "email")

changeAvatarWidget :: Widget
changeAvatarWidget = do
  let photos = [ (img_avatar_avatar_jpg, "Photo 1"::Text)
               , (img_avatar_avatar2_jpg, "Photo 2")
               , (img_avatar_avatar3_jpg, "Photo 3")
               , (img_avatar_avatar4_jpg, "Photo 4")
               , (img_avatar_avatar5_jpg, "Photo 5")
               , (img_avatar_avatar6_jpg, "Photo 6")
               , (img_avatar_avatar7_jpg, "Photo 7")
               , (img_avatar_avatar8_jpg, "Photo 8")
               , (img_avatar_avatar9_jpg, "Photo 9")
               ]
      avatar = head photos
  $(widgetFile "change-avatar")

uploadPhotoWidget :: Widget
uploadPhotoWidget = do
  (w, e) <- lift $ generateFormPost $ fileForm Nothing
  $(widgetFile "upload-photos")

editProfileWidget :: Widget
editProfileWidget = do
  (w, e) <- lift $ generateFormPost $ profileForm Nothing
  $(widgetFile "edit-profile")

profileWidget :: Widget
profileWidget= do
  (modal1, modal2, modal3) <- lift $ (,,) <$> newIdent <*> newIdent <*> newIdent
  $(widgetFile "profile")

postAccountIdR :: Handler ()
postAccountIdR = do
  ((r, _), _) <- runFormPost $ accountForm Nothing
  case r of
    FormSuccess x -> do
      liftIO $ putStrLn $ "[TODO] update user account " ++ T.unpack x
      setMessage "Update account..."
    _ -> setMessage "Fail to update"
  redirect ((HOME HomeR), [("tab", "account-id")])

postPasswordR :: Handler ()
postPasswordR = do
  ((r, _), _) <- runFormPost $ passwordForm Nothing
  case r of
    FormSuccess x -> do
      liftIO $ putStrLn $ "[TODO] update password " ++ T.unpack x
      setMessage "Update password..."
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessage "Fail to Update"
  redirect ((HOME HomeR), [("tab", "password")])

postEmailR :: Handler ()
postEmailR = do
  ((r, _), _) <- runFormPost $ emailForm [] Nothing
  case r of
    FormSuccess x -> do
      liftIO $ putStrLn $ "[TODO] send email reminder " ++ T.unpack x
      setMessage "Send email reminder"
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessage "Fail to send"
  redirect ((HOME HomeR), [("tab", "email")])
