module Handler.Help 
       ( getHelpR
       , postPasswordResetR
       ) where

import Import
import qualified Data.Text as T
import Owl.Helpers.Form (accountForm)

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
      liftIO $ putStrLn $ "[TODO] send reminder mail to " ++ T.unpack uname
      setMessage "Send reminder mail..."
      redirect ((HELP HelpR), [("tab", "password-reset")])
    _ -> redirect ((HELP HelpR), [("tab", "password-reset")])
