{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home 
       ( getHomeR
       , postPasswordR
       , postEmailR
       , getVerifyR
       , postVerifyR
       , postProfileR
       ) where

import Import
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import Network.Mail.Mime
import Yesod.Auth.HashDB (setPassword)
import Owl.Helpers.Form
import Owl.Helpers.Util (newIdent3, toGravatarHash)
import Owl.Helpers.Widget
import qualified Settings (owlEmailAddress)
import System.Random (newStdGen)
import Text.Shakespeare.Text (stext)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

getHomeR :: Handler Html
getHomeR = do
  u <- requireAuth
  (menuProfile, menuPassword, menuEmail) <- newIdent3
  tabIs <- fmap (maybe ("profile"==) (==)) $ lookupGetParam "tab"
  mmsg <- getMessage
  let passform = passwordForm' (entityVal u) Nothing
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")

postPasswordR :: Handler ()
postPasswordR = do
  u <- requireAuth
  ((r, _), _) <- runFormPost $ passwordForm' (entityVal u) Nothing
  case r of
    FormSuccess newPass -> do
      now <- liftIO getCurrentTime
      runDB . replace (entityKey u) =<< setPassword newPass (entityVal u) { userUpdated = now }
      setMessageI MsgPasswordUpdated
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToUpdatePassword
  redirect (HOME HomeR, [("tab", "password")])

postEmailR :: Handler ()
postEmailR = do
  uid <- requireAuthId
  ((r, _), _) <- runFormPost $ emailForm Nothing
  case r of
    FormSuccess email -> do
      register uid email
      setMessageI MsgSentVerifyMail
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailSentVerifyMail
  redirect (HOME HomeR, [("tab", "email")])

register :: UserId -> Text -> Handler ()
register uid email = do
  r <- getMessageRender
  verKey <- liftIO randomKey
  verUrl <- do
    render <- getUrlRenderParams
    return $ render (HOME VerifyR) [("key", verKey)]
  now <- liftIO getCurrentTime
  runDB $ update uid [ UserEmail =. Just email
                     , UserVerkey =. Just verKey
                     , UserVerstatus =. Just Unverified
                     , UserUpdated =. now
                     ]
  liftIO $ sendRegister r email verUrl
  where
    randomKey :: IO Text
    randomKey = do
      stdgen <- newStdGen
      return $ T.pack $ fst $ randomString 10 stdgen

sendRegister :: (AppMessage -> Text) -> Text -> Text -> IO ()
sendRegister render addr verurl =
  renderSendMail =<< simpleMail (to addr) Settings.owlEmailAddress sbj textPart' htmlPart' []
  where
    to = Address Nothing
    sbj = render MsgVerifyYourEmailAddress
    textPart' = [stext|
 #{render MsgConfirmEmailAddress}

 #{verurl}

 #{render MsgThankyou}
|]
    htmlPart' = TLE.decodeUtf8 $ renderHtml [shamlet|
<p>#{render MsgConfirmEmailAddress}
<p>
  <a href=#{verurl}>#{verurl}
<p>#{render MsgThankyou}
|]

getVerifyR :: Handler Html
getVerifyR = do
  uid <- requireAuthId
  memail <- runDB $ do
    u <- get404 uid
    return $ userEmail u
  Just verKey <- lookupGetParam "key"
  let params = [("key", verKey)]
  defaultLayout $ do
    setTitle "Verify email"
    $(widgetFile "verify-email")

postVerifyR :: Handler ()
postVerifyR = do
  uid <- requireAuthId
  ((r, _), _) <- runFormPost $ verifyForm Nothing
  case r of
    FormSuccess verKey -> do
      runDB $ do
        u <- get404 uid
        if userVerkey u == Just verKey
          then do
          lift $ setMessageI MsgSuccessVerifyEmail
          let hash' = fmap toGravatarHash $ userEmail u
          now <- liftIO getCurrentTime
          update uid [ UserVerkey =.Nothing
                     , UserVerstatus =. Just Verified
                     , UserMd5hash =. hash'
                     , UserUpdated =. now
                     ]
          else do
          lift $ setMessageI MsgFailVerifyEmail
          return ()
    _ -> do
      setMessageI MsgFailVerifyEmail
  redirect (HOME HomeR, [("tab", "email")])

postProfileR :: Handler ()
postProfileR = do
  uid <- requireAuthId
  ((r, _), _) <- runFormPost $ profileForm Nothing
  case r of
    FormSuccess (fn, gn, cmt) -> do
      now <- liftIO getCurrentTime
      runDB $ update uid [ UserFamilyname =. fn
                         , UserGivenname =. gn
                         , UserComment =. cmt
                         , UserUpdated =. now
                         ]
      setMessageI MsgUpdateProfile
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailUpdateProfile
  redirect (HOME HomeR, [("tab", "profile")])
