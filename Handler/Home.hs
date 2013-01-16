{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home 
       ( getHomeR
       , postAccountIdR
       , postPasswordR
       , postEmailR
       , getVerifyR
       , postVerifyR
       , postProfileR
       ) where

import Import
import Yesod.Auth
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import Network.Mail.Mime
import Owl.Helpers.Auth.HashDB (setPassword)
import Owl.Helpers.Form
import Owl.Helpers.Util (newIdent4)
import Owl.Helpers.Widget
import qualified Settings (owlEmailAddress)
import System.Random (newStdGen)
import Text.Shakespeare.Text (stext)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)

getHomeR :: Handler RepHtml
getHomeR = do
  u <- requireAuth
  (menuAccount, menuPassword, menuEmail, menuProfile) <- newIdent4
  tabIs <- fmap (maybe ("account-id"==) (==)) $ lookupGetParam "tab"
  mmsg <- getMessage
  defaultLayout $ do
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
  u <- requireAuth
  ((r, _), _) <- runFormPost $ passwordForm (entityVal u) Nothing
  case r of
    FormSuccess newPass -> do
      runDB . replace (entityKey u) =<< setPassword newPass (entityVal u)
      setMessageI MsgPasswordUpdated
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToUpdatePassword
  redirect ((HOME HomeR), [("tab", "password")])

postEmailR :: Handler ()
postEmailR = do
  uid <- requireAuthId
  ((r, _), _) <- runFormPost $ emailForm Nothing [] Nothing
  case r of
    FormSuccess email -> do
      register uid email
      setMessageI MsgSentVerifyMail
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailSentVerifyMail
  redirect ((HOME HomeR), [("tab", "email")])

register :: UserId -> Text -> Handler ()
register uid email = do
  verKey <- liftIO randomKey
  verUrl <- do
    render <- getUrlRenderParams
    tm <- getRouteToMaster
    return $ render (tm (HOME VerifyR)) [("key", verKey)]
  runDB $ update uid [ UserEmail =. Just email
                     , UserVerkey =. Just verKey
                     , UserVerstatus =. Just Unverified
                     ]
  liftIO $ sendRegister email verUrl
  where
    randomKey :: IO Text
    randomKey = do
      stdgen <- newStdGen
      return $ T.pack $ fst $ randomString 10 stdgen

sendRegister :: Text -> Text -> IO ()
sendRegister addr verurl = do
  mail <- simpleMail (to addr) Settings.owlEmailAddress sbj textPart htmlPart []
  renderSendMail mail
  where
    to = Address Nothing
    sbj = "Verify your email address"
    textPart = [stext|
Please confirm your email address by clicking on the link below.

\#{verurl}

Thank you
|]
    htmlPart = TLE.decodeUtf8 $ renderHtml [shamlet|
<p>Please confirm your email address by clicking on the link below.
<p>
  <a href=#{verurl}>#{verurl}
<p>Thank you
|]

getVerifyR :: Handler RepHtml
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
          update uid [UserVerkey =.Nothing, UserVerstatus =. Just Verified]
          else do
          lift $ setMessageI MsgFailVerifyEmail
          return ()
    _ -> do
      setMessageI MsgFailVerifyEmail
  redirect ((HOME HomeR), [("tab", "email")])

postProfileR :: Handler ()
postProfileR = do
  ((r, _), _) <- runFormPost $ profileForm Nothing
  case r of
    FormSuccess x -> do
      liftIO $ putStrLn $ "[TODO] Update profile! " ++ show x
      setMessage "Update profile"
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessage "fail to update profile"
  redirect ((HOME HomeR), [("tab", "profile")])
