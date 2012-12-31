module Handler.Help 
       ( getHelpR
       ) where

import Import
import Data.Maybe (isJust)

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
  defaultLayout $ do
    let usage = $(widgetFile "usage")
    setTitle "Help"
    $(widgetFile "help")

passreset :: Widget
passreset = do
  (w, e) <- lift $ generateFormPost $ emailForm Nothing
  $(widgetFile "password-reset")
