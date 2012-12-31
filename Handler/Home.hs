{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Prelude (head, tail)
import Yesod.Auth
import Data.Maybe (isJust)
import qualified Data.Text as T

accountForm :: Maybe Text -> Html -> MForm App App (FormResult Text, Widget)
accountForm mv fragment = do
  (res, view) <- mreq textField fs mv
  let widget = [whamlet|
\#{fragment}
<div .control-group.info .clearfix :fvRequired view:.required :not $ fvRequired view:.optional :isJust $ fvErrors view:.error>
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
    fs = FieldSettings { fsLabel = SomeMessage MsgAccountID
                       , fsTooltip = Nothing
                       , fsId = Nothing
                       , fsName = Nothing
                       , fsAttrs = [("class", "span2")]
                       }

getHomeR :: Handler RepHtml
getHomeR = do
  u <- requireAuth
  (menu1, menu2, menu3, menu4) <- (,,,) <$> newIdent <*> newIdent <*> newIdent <*> newIdent
  (modal1, modal2, modal3) <- (,,) <$> newIdent <*> newIdent <*> newIdent
  (w, e) <- generateFormPost $ accountForm Nothing  
  tabIs <- fmap (maybe ("account-id"==) (==)) $ lookupGetParam "tab"
  mmsg <- getMessage
  let photos = [ (img_avatar_avatar_jpg, "Photo 1"::Text)
               , (img_avatar_avatar2_jpg, "Photo 2")
               , (img_avatar_avatar3_jpg, "Photo 3")
               , (img_avatar_avatar4_jpg, "Photo 4")
               , (img_avatar_avatar5_jpg, "Photo 5")
               , (img_avatar_avatar6_jpg, "Photo 6")
               , (img_avatar_avatar7_jpg, "Photo 7")
               , (img_avatar_avatar8_jpg, "Photo 8")
               , (img_avatar_avatar9_jpg, "Photo 9")
               ]
      avatar = head photos
  defaultLayout $ do
    let accountId = $(widgetFile "account-id")
        password = $(widgetFile "password")
        email = $(widgetFile "email")
        profile = $(widgetFile "profile")
        changeAvatar = $(widgetFile "change-avatar")
        uploadPhotos = $(widgetFile "upload-photos")
        editProf = $(widgetFile "edit-profile")
    setTitle "Home"
    $(widgetFile "homepage")

postAccountIdR :: Handler ()
postAccountIdR = do
  ((r, _), _) <- runFormPost $ accountForm Nothing
  case r of
    FormSuccess x -> do
      liftIO $ putStrLn $ "[TODO] update user account " ++ T.unpack x
      setMessage "Update account..."
      redirect ((HOME HomeR), [("tab", "account-id")])
    _ -> redirect ((HOME HomeR), [("tab", "account-id")])
