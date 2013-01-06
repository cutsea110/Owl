module Owl.Helpers.Form 
       ( accountForm
       , passwordForm
       , emailForm
       , profileForm
       , fileForm
       ) where

import Import
import Data.Maybe (isJust)
import Data.Tuple.HT (fst3, snd3, thd3)
import Text.Julius (rawJS)

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
        (FormSuccess _, FormSuccess y, FormSuccess z) ->
          if y == z
          then FormSuccess y
          else FormFailure ["don't match between new password and confirmation"]
        _ -> FormFailure ["fail to password update!!"]
      vks = [(view0,"info"::Text),(view1,"warning"),(view2,"warning")]
  let widget = [whamlet|
\#{fragment}
$forall (v, k) <- vks
  <div .control-group.#{k} .clearfix :fvRequired v:.required :not $ fvRequired v:.optional :isJust $ fvErrors v:.error>
    <label .control-label for=#{fvId v}>#{fvLabel v}
    <div .controls .input>
      ^{fvInput v}
      $maybe tt <- fvTooltip v
        <span .help-block>#{tt}
      $maybe err <- fvErrors v
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

emailForm :: [(Text, Text)] -> Maybe Text -> Html -> MForm App App (FormResult Text, Widget)
emailForm attrs mv fragment = do
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
                       , fsAttrs = attrs
                       }

profileForm :: Maybe (Text, Text, Maybe Textarea) -> Html -> MForm App App (FormResult (Text, Text, Maybe Textarea), Widget)
profileForm mv fragment = do
  (res0, view0) <- mreq textField (fs MsgFamilyName) (fst3 <$> mv)
  (res1, view1) <- mreq textField (fs MsgGivenName) (snd3 <$> mv)
  (res2, view2) <- mopt textareaField (fs MsgProfile) (thd3 <$> mv)
  let res = case (res0, res1, res2) of
        (FormSuccess x, FormSuccess y, FormSuccess z) -> FormSuccess (x, y, z)
        _ -> FormFailure ["fail to profile update!"]
      vks = [(view0, "success"::Text), (view1, "success"), (view2, "info")]
  let widget = [whamlet|
\#{fragment}
$forall (v, k) <- vks
  <div .control-group.#{k}.clearfix :fvRequired v:.required :not $ fvRequired v:.optional :isJust $ fvErrors v:.error>
    <label .control-label for=#{fvId v}>#{fvLabel v}
    <div .controls .input>
      ^{fvInput v}
      $maybe tt <- fvTooltip v
        <span .help-block>#{tt}
      $maybe err <- fvErrors v
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

fileForm :: Maybe FileInfo -> Html -> MForm App App (FormResult FileInfo, Widget)
fileForm mv fragment = do
  (res, view) <- mreq fileField' fs mv
  let widget = [whamlet|
\#{fragment}
<div .control-group.clearfix :fvRequired view:.required :not $ fvRequired view:.optional :isJust $ fvErrors view:.error>
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
    fs = FieldSettings { fsLabel = SomeMessage MsgPhotoPath
                       , fsTooltip = Nothing
                       , fsId = Nothing
                       , fsName = Nothing
                       , fsAttrs = []
                       }

fileField' :: Field App App FileInfo
fileField' = fileField
    { fieldView = \id' name attrs _ isReq -> do
       toWidget [julius|
$("##{rawJS id'}-browse, ##{rawJS id'}-custom").click(function(){
  $("##{rawJS id'}").click();
});
$("##{rawJS id'}").change(function(){
  $("##{rawJS id'}-custom").text($(this).val());
});
|]
       [whamlet|
<div .input-append>
  <span id=#{id'}-custom .input-large.uneditable-input>
  <input id=#{id'} name=#{name} .hide *{attrs} type=file :isReq:required>
  <a .btn id=#{id'}-browse>_{MsgBrowse}
  <button .btn.btn-primary><i class="icon-upload icon-white"></i> _{MsgUpload}
|]
    }
