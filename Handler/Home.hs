{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Prelude (head, tail)
import Yesod.Auth
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Tuple.HT (fst3, snd3, thd3)

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

passwordForm :: Maybe (Text,Text,Text) -> Html -> MForm App App (FormResult Text, Widget)
passwordForm mv fragment = do
  (res0, view0) <- mreq passwordField (fs MsgCurrentPassword) (fst3 <$> mv)
  (res1, view1) <- mreq passwordField (fs MsgNewPassword) (snd3 <$> mv)
  (res2, view2) <- mreq passwordField (fs MsgConfirmPassword) (thd3 <$> mv)
  let res = case (res0, res1, res2) of
        (FormSuccess x, FormSuccess y, FormSuccess z) ->
          if y == z
          then FormSuccess y
          else FormFailure ["don't match between new password and confirmation"]
        _ -> FormFailure ["fail to password update!!"]
  liftIO $ putStrLn $ show (res0, res1, res2)
  let widget = [whamlet|
\#{fragment}
<div .control-group.info .clearfix :fvRequired view0:.required :not $ fvRequired view0:.optional :isJust $ fvErrors view0:.error>
  <label .control-label for=#{fvId view0}>#{fvLabel view0}
  <div .controls .input>
    ^{fvInput view0}
    $maybe tt <- fvTooltip view0
      <span .help-block>#{tt}
    $maybe err <- fvErrors view0
      <span .help-block>#{err}

<div .control-group.warning .clearfix :fvRequired view1:.required :not $ fvRequired view1:.optional :isJust $ fvErrors view1:.error>
  <label .control-label for=#{fvId view1}>#{fvLabel view1}
  <div .controls .input>
    ^{fvInput view1}
    $maybe tt <- fvTooltip view1
      <span .help-block>#{tt}
    $maybe err <- fvErrors view1
      <span .help-block>#{err}

<div .control-group.warning .clearfix :fvRequired view2:.required :not $ fvRequired view2:.optional :isJust $ fvErrors view2:.error>
  <label .control-label for=#{fvId view2}>#{fvLabel view2}
  <div .controls .input>
    ^{fvInput view2}
    $maybe tt <- fvTooltip view2
      <span .help-block>#{tt}
    $maybe err <- fvErrors view2
      <span .help-block>#{err}
|]
  return (res, widget)
  where
    fs l = FieldSettings { fsLabel = SomeMessage l
                         , fsTooltip = Nothing
                         , fsId = Nothing
                         , fsName = Nothing
                         , fsAttrs = []
                         }

getHomeR :: Handler RepHtml
getHomeR = do
  u <- requireAuth
  (menu1, menu2, menu3, menu4) <- (,,,) <$> newIdent <*> newIdent <*> newIdent <*> newIdent
  (modal1, modal2, modal3) <- (,,) <$> newIdent <*> newIdent <*> newIdent
  (w, e) <- generateFormPost $ accountForm Nothing
  (wp, ep) <- generateFormPost $ passwordForm Nothing
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

postPasswordR :: Handler ()
postPasswordR = do
  ((r, _), _) <- runFormPost $ passwordForm Nothing
  case r of
    FormSuccess x -> do
      liftIO $ putStrLn $ "[TODO] update password " ++ T.unpack x
      setMessage "Update password..."
      redirect ((HOME HomeR), [("tab", "password")])
    FormFailure (x:_) -> do
      setMessage x
      redirect ((HOME HomeR), [("tab", "password")])
