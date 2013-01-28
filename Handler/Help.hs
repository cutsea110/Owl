module Handler.Help 
       ( getHelpR
       , postPasswordResetR
       ) where

import Import
import qualified Data.Text as T
import Yesod.Auth (requireAuth)
import Owl.Helpers.Form (emailForm)

getHelpR :: Handler RepHtml
getHelpR = do
  (menuSendReminderMail, menuUsage) <- (,) <$> newIdent <*> newIdent
  (w, e) <- generateFormPost $ emailForm [("class", "span3")] Nothing Nothing
  tabIs <- fmap (maybe ("password-reset"==) (==)) $ lookupGetParam "tab"
  mmsg <- getMessage
  defaultLayout $ do
    let passreset = $(widgetFile "password-reset")
        usage = $(widgetFile "usage")
    setTitle "Help"
    $(widgetFile "help")

postPasswordResetR :: Handler ()
postPasswordResetR = do
  u <- requireAuth
  let (u', memail, mverstatus) = (entityVal u, userEmail u', userVerstatus u')
  ((r, _), _) <- runFormPost $ emailForm [] mverstatus memail
  case r of
    FormSuccess x -> do
      liftIO $ putStrLn $ "[TODO] send reminder mail to " ++ T.unpack x
      setMessage "Send reminder mail..."
      redirect ((HELP HelpR), [("tab", "password-reset")])
    _ -> redirect ((HELP HelpR), [("tab", "password-reset")])
