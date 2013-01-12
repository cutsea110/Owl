{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home 
       ( getHomeR
       , postAccountIdR
       , postPasswordR
       , postEmailR
       , postProfileR
       ) where

import Import
import Yesod.Auth
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.Mail.Mime (randomString, simpleMail, renderSendMail, Address(..))
import qualified Settings (owlEmailAddress)
import System.Random (newStdGen)
import Owl.Helpers.Auth.HashDB (setPassword)
import Owl.Helpers.Form
import Owl.Helpers.Util (newIdent4)
import Owl.Helpers.Widget

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
  ((r, _), _) <- runFormPost $ emailForm [] Nothing
  case r of
    FormSuccess email -> do
      register uid email
      setMessage "Send email reminder"
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessage "Fail to send"
  redirect ((HOME HomeR), [("tab", "email")])

register :: UserId -> Text -> Handler ()
register uid email = do
  verKey <- liftIO randomKey
  runDB $ update uid [ UserEmail =. Just email
                     , UserVerkey =. Just verKey
                     , UserVerstatus =. Just Unverified
                     ]
  liftIO $ sendRegister email verKey
  where
    randomKey :: IO Text
    randomKey = do
      stdgen <- newStdGen
      return $ T.pack $ fst $ randomString 10 stdgen

sendRegister :: Text -> Text -> IO ()
sendRegister addr key = do
  mail <- simpleMail (to addr) Settings.owlEmailAddress sbj body html []
  renderSendMail mail
  where
    to = Address Nothing
    key' = LT.pack $ T.unpack key
    (sbj, body, html) = 
      ("Confirm mail", "Confirmation mail <br>key: " `LT.append` key', htmlize body)
    htmlize id = id


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
