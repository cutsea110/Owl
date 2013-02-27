module Handler.Help 
       ( getHelpR
       , postPasswordResetR
       ) where

import Import
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time
import Network.Mail.Mime
import Owl.Helpers.Form (accountForm)
import qualified Settings (owlEmailAddress)
import System.Random (newStdGen)
import Text.Shakespeare.Text (stext)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)

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
  (onepass, now) <- liftIO $ (,) <$> randomKey <*> getCurrentTime
  _ <- runDB $ insert $ Onetime uid onepass now
  liftIO $ sendRegister r uname onepass email
  where
    randomKey :: IO Text
    randomKey = do
      stdgen <- newStdGen
      return $ T.pack $ fst $ randomString 10 stdgen

sendRegister :: (AppMessage -> Text) -> Text -> Text -> Text -> IO ()
sendRegister render uname pass addr =
  renderSendMail =<< simpleMail (to addr) Settings.owlEmailAddress sbj textPart htmlPart []
  where
    to = Address Nothing
    sbj = render MsgResetYourPassword
    textPart = [stext|
\#{render MsgLoginByOnetimepass}

\#{render MsgAccountID} : #{uname}
\#{render MsgOnetimePassword} : #{pass}

\#{render MsgIfYouDontRequestOnetimepassMail}
|]
    htmlPart = TLE.decodeUtf8 $ renderHtml [shamlet|
<p>#{render MsgLoginByOnetimepass}
<dl>
  <dt>#{render MsgAccountID}
  <dd>#{uname}
  <dt>#{render MsgOnetimePassword}
  <dd>#{pass}

<p>#{render MsgIfYouDontRequestOnetimepassMail}
|]
