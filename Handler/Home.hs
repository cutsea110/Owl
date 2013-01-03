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
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Tuple.HT (fst3, snd3, thd3)
import Text.Julius (rawJS)
import Owl.Helpers.Form

getHomeR :: Handler RepHtml
getHomeR = do
  u <- requireAuth
  (menu1, menu2, menu3, menu4) <- (,,,) <$> newIdent <*> newIdent <*> newIdent <*> newIdent
  (modal1, modal2, modal3) <- (,,) <$> newIdent <*> newIdent <*> newIdent
  (wa, ea) <- generateFormPost $ accountForm Nothing
  (wp, ep) <- generateFormPost $ passwordForm Nothing
  (we, ee) <- generateFormPost $ emailForm  [("class", "span3"),("placeholder","cutsea110@gmail.com")] Nothing
  (wi, ei) <- generateFormPost $ profileForm Nothing
  (wu, eu) <- generateFormPost $ fileForm Nothing
  tabIs <- fmap (maybe ("account-id"==) (==)) $ lookupGetParam "tab"
  mmsg <- getMessage
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
  defaultLayout $ do
    let accountId = $(widgetFile "account-id")
        password = $(widgetFile "password")
        email = $(widgetFile "email")
        profile = $(widgetFile "profile")
        changeAvatar = $(widgetFile "change-avatar")
        uploadPhotos = $(widgetFile "upload-photos")
        editProf = $(widgetFile "edit-profile")
    setTitle "Home"
    $(widgetFile "homepage")

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
