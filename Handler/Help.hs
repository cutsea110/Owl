module Handler.Help 
       ( getHelpR
       , postPasswordResetR
       , getOnetimeLoginR
       , postOnetimeLoginR
       , getResetPasswordR
       , postResetPasswordR
       ) where

import Import
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time
import Network.Mail.Mime
import Owl.Helpers.Auth.HashDB (setPassword)
import Owl.Helpers.Form (accountForm, passwordForm, onetimeForm)
import qualified Settings (owlEmailAddress)
import System.Random (newStdGen)
import Text.Shakespeare.Text (stext)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Yesod.Auth

getHelpR :: Handler RepHtml
getHelpR = do
  (menuSendReminderMail, menuUsage) <- (,) <$> newIdent <*> newIdent
  (w, e) <- generateFormPost $ accountForm Nothing
  tabIs <- fmap (maybe ("usage"==) (==)) $ lookupGetParam "tab"
  mmsg <- getMessage
  defaultLayout $ do
    let passreset = $(widgetFile "password-reset")
        usage = $(widgetFile "usage")
    setTitle "Help"
    $(widgetFile "help")

postPasswordResetR :: Handler ()
postPasswordResetR = do
  ((r, _), _) <- runFormPost $ accountForm Nothing
  case r of
    FormSuccess uname -> do
      mu <- runDB $ getBy $ UniqueUser uname
      case mu of
        Just u -> case userEmail $ entityVal u of
          Just email -> do
            registOnetimePassword (entityKey u) (userUsername $ entityVal u) email
            setMessageI MsgSendPasswordReset
          Nothing -> setMessageI MsgCannotSendPasswordReset
        Nothing -> setMessageI MsgCannotFindAccount
      redirect ((HELP HelpR), [("tab", "password-reset")])
    _ -> redirect ((HELP HelpR), [("tab", "password-reset")])

registOnetimePassword :: UserId -> Text -> Text -> Handler ()
registOnetimePassword uid uname email = do
  r <- getMessageRender
  (onepass, limit) <- liftIO $ (,)
                      <$> randomKey 
                      <*> fmap (addUTCTime 86400) getCurrentTime
  _ <- runDB $ insert $ Onetime uid onepass limit
  url <- do
    render <- getUrlRender
    tm <- getRouteToMaster
    return $ render (tm $ HELP OnetimeLoginR)
  liftIO $ sendRegister r url uname onepass email
  where
    randomKey :: IO Text
    randomKey = do
      stdgen <- newStdGen
      return $ T.pack $ fst $ randomString 10 stdgen

sendRegister :: (AppMessage -> Text) -> Text -> Text -> Text -> Text -> IO ()
sendRegister render url uname pass addr = do
  renderSendMail =<< simpleMail (to addr) Settings.owlEmailAddress sbj textPart htmlPart []
  where
    to = Address Nothing
    sbj = render MsgResetYourPassword
    textPart = [stext|
 #{render MsgLoginByOnetimepass}

 #{render MsgOnetimeLoginURL}: #{url}
 #{render MsgAccountID} : #{uname}
 #{render MsgOnetimePassword} : #{pass}

 #{render MsgIfYouDontRequestOnetimepassMail}
|]
    htmlPart = TLE.decodeUtf8 $ renderHtml [shamlet|
<p>#{render MsgLoginByOnetimepass}
<p>#{render MsgOnetimeLoginURL} :
  <a href=#{url}>#{url}
<dl>
  <dt>#{render MsgAccountID}
  <dd>#{uname}
  <dt>#{render MsgOnetimePassword}
  <dd>#{pass}

<p>#{render MsgIfYouDontRequestOnetimepassMail}
|]

getOnetimeLoginR :: Handler RepHtml
getOnetimeLoginR = do
  (w, e) <- generateFormPost $ onetimeForm Nothing
  mmsg <- getMessage
  defaultLayout $ do
    setTitle "Onetime login"
    $(widgetFile "onetime-login")

postOnetimeLoginR :: Handler ()
postOnetimeLoginR = do
  ((r, _), _) <- runFormPost $ onetimeForm Nothing
  case r of
    FormSuccess (uname, otp) -> do
      mu <- runDB $ getBy $ UniqueUser uname
      case mu of
        Just u -> do
          now <- liftIO getCurrentTime
          c <- runDB $ count [ OnetimeUser ==. entityKey u
                             , OnetimePassword ==. otp
                             , OnetimeLimit >=. now
                             ]
          if c > 0
            then do
            setCreds False $ Creds "onetime" uname [("pass", otp)]
            setMessageI MsgSuccessLoginByOnetimePassword
            redirect $ HELP ResetPasswordR
            else do
            setMessageI MsgFailLoginByOnetimePassword
            redirect $ HELP OnetimeLoginR
        Nothing -> do
          setMessageI MsgFailLoginByOnetimePassword
          redirect $ HELP OnetimeLoginR
    _ -> do
      setMessageI MsgFailLoginByOnetimePassword
      redirect $ HELP OnetimeLoginR

getResetPasswordR :: Handler RepHtml
getResetPasswordR = do
  (w, e) <- generateFormPost $ passwordForm Nothing
  mmsg <- getMessage
  defaultLayout $ do
    setTitle "Reset password"
    $(widgetFile "reset-password")

postResetPasswordR :: Handler ()
postResetPasswordR = do
  uid <- requireAuthId
  ((r, _), _) <- runFormPost $ passwordForm Nothing
  case r of
    FormSuccess newPass -> do
      runDB $ do
        u <- get404 uid
        now <- liftIO getCurrentTime
        replace uid =<< setPassword newPass u { userUpdated = now }
      setMessageI MsgPasswordUpdated
    FormFailure (x:_) -> setMessage $ toHtml x
    _ -> setMessageI MsgFailToUpdatePassword
  redirect $ HELP ResetPasswordR
