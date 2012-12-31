module Handler.Help 
       ( getHelpR
       ) where

import Import
import Text.Julius (rawJS)
import Data.Text (Text)

getHelpR :: Handler RepHtml
getHelpR = do
  defaultLayout $ do
    let usage = $(widgetFile "usage")
    setTitle "Help"
    $(widgetFile "help")

passreset :: Widget
passreset = do
  ident <- lift newIdent
  (w, e) <- lift $ generateFormPost $ renderBootstrap $ ef ident
  $(widgetFile "password-reset")

ef :: (RenderMessage master AppMessage, RenderMessage master FormMessage) =>
      Text -> AForm sub master Text
ef x = areq emailField fs Nothing
  where
    fs = FieldSettings { fsLabel = SomeMessage MsgEmail
                       , fsTooltip = Nothing
                       , fsId = Just x
                       , fsName = Nothing
                       , fsAttrs = [("class", "span3")]
                       }
