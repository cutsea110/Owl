module Owl.Helpers.Form 
       ( accountForm
       , accountPasswordForm
       , passwordForm
       , passwordConfirmForm
       , emailForm
       , userEmailForm
       , verifyForm
       , profileForm
       , fileForm
       ) where

import Import
import Control.Arrow ((&&&))
import Data.Maybe (isJust)
import qualified Data.Text as T (pack)
import Data.Tuple.HT (fst3, snd3, thd3)
import Owl.Helpers.Auth.HashDB (validateUser)
import Text.Julius (rawJS)

fs :: RenderMessage m msg => msg -> FieldSettings m
fs msg = FieldSettings { fsLabel = SomeMessage msg
                       , fsTooltip = Nothing
                       , fsId = Nothing
                       , fsName = Nothing
                       , fsAttrs = []
                       }


accountForm :: Maybe Text -> Html -> MForm s App (FormResult Text, GWidget s App ())
accountForm mv = renderBootstrap $ areq textField (fs MsgAccountID) mv

accountPasswordForm :: Maybe (Text, Text, Text) -> Form (Text, Text)
accountPasswordForm mv fragment = do
  (res0, view0) <- mreq textField (fs MsgAccountID) (fst3 <$> mv)
  (res1, view1) <- mreq passwordField (fs MsgPassword) (snd3 <$> mv)
  (res2, view2) <- mreq passwordField (fs MsgConfirmPassword) (thd3 <$> mv)
  res <- lift $ case (res0, res1, res2) of
    (FormSuccess uname, FormSuccess newPass, FormSuccess newPass') -> do
      return $
        if newPass == newPass'
        then FormSuccess (uname, newPass)
        else FormFailure ["don't match between new password and confirmation."]
    _ -> return $ FormFailure ["fail to create user"]
  let vks = [(view0, "info"::Text), (view1, "info"), (view2, "info")]
      widget = [whamlet|
\#{fragment}
$forall (v, k) <- vks
  <div .control-group.#{k} .clearfix :fvRequired v:.required :not $ fvRequired v:.optional :isJust $ fvErrors v:.error>
    <label .control-label for=##{fvId v}>#{fvLabel v}
    <div .controls .input>
      ^{fvInput v}
      $maybe tt <- fvTooltip v
        <span .help-block>#{tt}
      $maybe err <- fvErrors v
        <span .help-block>#{err}
|]
  return (res, widget)

passwordForm :: User -> Maybe (Text,Text,Text) -> Form Text
passwordForm u mv fragment = do
  (res0, view0) <- mreq passwordField (fs MsgCurrentPassword) (fst3 <$> mv)
  (res1, view1) <- mreq passwordField (fs MsgNewPassword) (snd3 <$> mv)
  (res2, view2) <- mreq passwordField (fs MsgConfirmNewPassword) (thd3 <$> mv)
  res <- lift $ case (res0, res1, res2) of
        (FormSuccess curPass, FormSuccess newPass, FormSuccess newPass') -> do
          checkedPass <- validateUser (mkUnique u) curPass
          return $
            if checkedPass
            then if newPass == newPass'
                 then FormSuccess newPass
                 else FormFailure ["don't match between new password and confirmation."]
            else FormFailure ["incorrect current password."]
        _ -> return $ FormFailure ["fail to password update."]
  let vks = [(view0,"info"::Text),(view1,"warning"),(view2,"warning")]
      widget = [whamlet|
\#{fragment}
$forall (v, k) <- vks
  <div .control-group.#{k} .clearfix :fvRequired v:.required :not $ fvRequired v:.optional :isJust $ fvErrors v:.error>
    <label .control-label for=##{fvId v}>#{fvLabel v}
    <div .controls .input>
      ^{fvInput v}
      $maybe tt <- fvTooltip v
        <span .help-block>#{tt}
      $maybe err <- fvErrors v
        <span .help-block>#{err}
|]
  return (res, widget)
  where
    mkUnique = UniqueUser . userUsername

passwordConfirmForm :: Maybe (Text,Text) -> Form Text
passwordConfirmForm mv fragment = do
  (res0, view0) <- mreq passwordField (fs MsgNewPassword) (fst <$> mv)
  (res1, view1) <- mreq passwordField (fs MsgConfirmNewPassword) (snd <$> mv)
  res <- lift $ case (res0, res1) of
        (FormSuccess newPass, FormSuccess newPass') -> do
          return $
            if newPass == newPass'
            then FormSuccess newPass
            else FormFailure ["don't match between new password and confirmation."]
        _ -> return $ FormFailure ["fail to password update."]
  let vs = [view0, view1]
      widget = do
        toWidget [julius|
$('button.edit-password').click(function(){
  var uri = $(this).attr('password-uri'),
      modalid = $(this).attr('href');
  $(modalid).find('div.edit-password form').attr('action', uri);
});
|]
        [whamlet|
\#{fragment}
$forall v <- vs
  <div .control-group.warning .clearfix :fvRequired v:.required :not $ fvRequired v:.optional :isJust $ fvErrors v:.error>
    <label .control-label for=##{fvId v}>#{fvLabel v}
    <div .controls .input>
      ^{fvInput v}
      $maybe tt <- fvTooltip v
        <span .help-block>#{tt}
      $maybe err <- fvErrors v
        <span .help-block>#{err}
|]
  return (res, widget)

emailForm :: [(Text, Text)] -> Maybe VerStatus -> Maybe Text -> Form Text
emailForm attrs vs mv fragment = do
  (res, view) <- mreq emailField fs mv
  let widget = [whamlet|
\#{fragment}
<div .control-group.warning .clearfix :fvRequired view:.required :not $ fvRequired view:.optional :isJust $ fvErrors view:.error>
  <label .control-label for=##{fvId view}>#{fvLabel view} #
    $maybe s <- vs
      <span .badge :s == Verified:.badge-success :s == Unverified:.badge-warning >#{show s}
    $nothing
      <span .badge.badge-warning>Unverified
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

userEmailForm :: [(Text, Text)] -> Maybe (Maybe Text, Maybe VerStatus, Maybe Text) -> Form (Maybe Text, Maybe VerStatus, Maybe Text)
userEmailForm attrs mv fragment = do
  (res0, view0) <- mopt emailField (fs MsgEmail) (fst3 <$> mv)
  (res1, view1) <- mopt (selectFieldList vss) (fs MsgVerstatus) (snd3 <$> mv)
  (res2, view2) <- mopt textField (fs MsgVerkey) (thd3 <$> mv)
  let res = case (res0, res1, res2) of
        (FormSuccess x, FormSuccess y, FormSuccess z) -> FormSuccess (x, y, z)
        _ -> FormFailure ["fail to email update!"]
      vks = [(view0, "info"::Text), (view1, "info"), (view2, "info")]
  let widget = do
        toWidget [julius|
$('button.edit-email').click(function(){
  var uri = $(this).attr('email-uri'),
      modalid = $(this).attr('href');
  $(modalid).find('div.edit-email form').attr('action', uri);
  $.getJSON(uri, null, function(data, status){
    if (status=='success') {
      $('##{rawJS $ fvId view0}').val(data.email);
      $('##{rawJS $ fvId view1}').val(data.verstatus);
      $('##{rawJS $ fvId view2}').val(data.verkey);
    } else {
      $('##{rawJS $ fvId view0}').val(null);
      $('##{rawJS $ fvId view1}').val('none');
      $('##{rawJS $ fvId view2}').val(null
);
    }
  });
});
|]
        [whamlet|
\#{fragment}
$forall (v, k) <- vks
  <div .control-group.#{k}.clearfix :fvRequired v:.required :not $ fvRequired v:.optional :isJust $ fvErrors v:.error>
    <label .control-label for=##{fvId v}>#{fvLabel v} #
    <div .controls .input>
      ^{fvInput v}
      $maybe tt <- fvTooltip v
        <span .help-block>#{tt}
      $maybe err <- fvErrors v
        <span .help-block>#{err}
|]
  return (res, widget)
  where
    vss :: [(Text, VerStatus)]
    vss = map ((T.pack . show) &&& id) [minBound..maxBound]

verifyForm :: Maybe Text -> Form Text
verifyForm mv fragment = do
  (res, view) <- mreq hiddenField "verkey" mv
  let widget = [whamlet|
\#{fragment}
<div .control-group .clearfix :fvRequired view:.required :not $ fvRequired view:.optional :isJust $ fvErrors view:.error>
  <div .controls .input>
    ^{fvInput view}
    $maybe tt <- fvTooltip view
      <span .help-block>#{tt}
    $maybe err <- fvErrors view
      <span .help-block>#{err}
|]
  return (res, widget)

profileForm :: Maybe (Text, Text, Maybe Textarea) -> Form (Text, Text, Maybe Textarea)
profileForm mv fragment = do
  (res0, view0) <- mreq textField (fs MsgFamilyName) (fst3 <$> mv)
  (res1, view1) <- mreq textField (fs MsgGivenName) (snd3 <$> mv)
  (res2, view2) <- mopt textareaField (fs MsgProfile) (thd3 <$> mv)
  let res = case (res0, res1, res2) of
        (FormSuccess x, FormSuccess y, FormSuccess z) -> FormSuccess (x, y, z)
        _ -> FormFailure ["fail to profile update!"]
      vks = [(view0, "success"::Text), (view1, "success"), (view2, "info")]
  let widget = do
        toWidget [julius|
$('button.edit-profile').click(function(){
  var uri = $(this).attr('profile-uri'),
      modalid = $(this).attr('href');

  // set action uri for post
  $(modalid).find('div.edit-profile form').attr('action', uri);
  $.getJSON(uri, null, function(data, status){
    if (status=='success') {
      // bind data
      $('##{rawJS $ fvId view0}').val(data.familyname);
      $('##{rawJS $ fvId view1}').val(data.givenname);
      $('##{rawJS $ fvId view2}').val(data.comment);
    } else {
      $('##{rawJS $ fvId view0},##{rawJS $ fvId view1},##{rawJS $ fvId view2}').val('');
    }
  });
});
|]
        [whamlet|
\#{fragment}
$forall (v, k) <- vks
  <div .control-group.#{k}.clearfix :fvRequired v:.required :not $ fvRequired v:.optional :isJust $ fvErrors v:.error>
    <label .control-label for=##{fvId v}>#{fvLabel v}
    <div .controls .input>
      ^{fvInput v}
      $maybe tt <- fvTooltip v
        <span .help-block>#{tt}
      $maybe err <- fvErrors v
        <span .help-block>#{err}
|]
  return (res, widget)

fileForm :: Maybe FileInfo -> Form FileInfo
fileForm mv fragment = do
  (res, view) <- mreq fileField' (fs MsgUploadFilePath) mv
  let widget = [whamlet|
\#{fragment}
<div .control-group.clearfix :fvRequired view:.required :not $ fvRequired view:.optional :isJust $ fvErrors view:.error>
  <label .control-label for=##{fvId view}>#{fvLabel view}
  <div .controls .input>
    ^{fvInput view}
    $maybe tt <- fvTooltip view
      <span .help-block>#{tt}
    $maybe err <- fvErrors view
      <span .help-block>#{err}
|]
  return (res, widget)

fileField' :: Field App App FileInfo
fileField' = fileField
    { fieldView = \id' name attrs _ isReq -> do
       toWidget [lucius|##{id'}-custom:hover {
  cursor: pointer;
}
|]
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
  <button type=submit .btn.btn-primary><i class="icon-upload icon-white"></i> _{MsgUpload}
|]
    }
