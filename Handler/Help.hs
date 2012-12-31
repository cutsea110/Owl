module Handler.Help 
       ( getHelpR
       , postPasswordResetR
       ) where

import Import
import Data.Maybe (isJust, maybe)
import qualified Data.Text as T

emailForm :: Maybe Text -> Html -> MForm App App (FormResult Text, Widget)
emailForm mv fragment = do
  (res, view) <- mreq emailField fs mv
  let widget = [whamlet|
\#{fragment}
<div .control-group.warning .clearfix :fvRequired view:.required :not $ fvRequired view:.optional :isJust $ fvErrors view:.error>
  <label .control-label for=#{fvId view}>#{fvLabel view}
  <div .controls .input>
    ^{fvInput view}
    $maybe tt <- fvTooltip view
      <span .help-block>#{tt}
    $maybe err <- fvErrors view
      <span .help-block>#{err}
|]
  return (res, widget)
  where
    fs = FieldSettings { fsLabel = SomeMessage MsgEmail
                       , fsTooltip = Nothing
                       , fsId = Nothing
                       , fsName = Nothing
                       , fsAttrs = [("class", "span3")]
                       }

getHelpR :: Handler RepHtml
getHelpR = do
  (menu1, menu2) <- (,) <$> newIdent <*> newIdent
  (w, e) <- generateFormPost $ emailForm Nothing
  tabIs <- fmap (maybe ("password-reset"==) (==)) $ lookupGetParam "tab"
  mmsg <- getMessage
  defaultLayout $ do
    let passreset = $(widgetFile "password-reset")
        usage = $(widgetFile "usage")
    setTitle "Help"
    $(widgetFile "help")

postPasswordResetR :: Handler ()
postPasswordResetR = do
  ((r, _), _) <- runFormPost $ emailForm Nothing
  case r of
    FormSuccess x -> do
      liftIO $ putStrLn $ "[TODO] send reminder mail to " ++ T.unpack x
      setMessage "Send reminder mail..."
      redirect ((HELP HelpR), [("tab", "password-reset")])
    _ -> redirect ((HELP HelpR), [("tab", "password-reset")])
