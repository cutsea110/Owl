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

passwordConfirmForm :: Maybe Text -> Html -> MForm s App (FormResult Text, GWidget s App ())
passwordConfirmForm mv = renderBootstrap $ areq passwordConfirmField "" mv

passwordConfirmField :: Field s App Text
passwordConfirmField = Field
  { fieldParse = \vals _ -> do
       case vals of
         [x, y] | x == y -> return $ Right $ Just x
                | otherwise -> return $ Left "Passwords don't match"
         [] -> return $ Right Nothing
         _ -> return $ Left "Incorrect number of results"
  , fieldView = \id' name attrs val isReq -> [whamlet|
<label.control-label for=##{id'}>_{MsgNewPassword}
<div.controls.input>
  <input ##{id'} name=#{name} *{attrs} type=password :isReq:required>
<label.control-label for=##{id'}-confirm>_{MsgConfirmNewPassword}
<div.controls.input>
  <input ##{id'}-confirm name=#{name} *{attrs} type=password :isReq:required>
|]
  , fieldEnctype = UrlEncoded
  }

emailForm :: Maybe Text -> Html -> MForm s App (FormResult Text, GWidget s App ())
emailForm mv = renderBootstrap $ areq emailField (fs MsgEmail) mv

userEmailForm :: Maybe (Maybe Text, Maybe VerStatus, Maybe Text) -> Html -> MForm s App (FormResult (Maybe Text, Maybe VerStatus, Maybe Text), GWidget s App ())
userEmailForm mv = renderBootstrap $ (,,)
                   <$> aopt emailField (fs MsgEmail) (fst3 <$> mv)
                   <*> aopt (selectFieldList vss) (fs MsgVerstatus) (snd3 <$> mv)
                   <*> aopt textField (fs MsgVerkey) (thd3 <$> mv)
  where
    vss :: [(Text, VerStatus)]
    vss = map ((T.pack . show) &&& id) [minBound..maxBound]

verifyForm :: Maybe Text -> Html -> MForm s App (FormResult Text, GWidget s App ())
verifyForm mv = renderBootstrap $ areq hiddenField "verkey" mv

profileForm :: Maybe (Text, Text, Maybe Textarea) -> Html -> MForm s App (FormResult (Text, Text, Maybe Textarea), GWidget s App ())
profileForm mv = renderBootstrap $ (,,) 
                 <$> areq textField (fs MsgFamilyName) (fst3 <$> mv)
                 <*> areq textField (fs MsgGivenName) (snd3 <$> mv)
                 <*> aopt textareaField (fs MsgProfile) (thd3 <$> mv)

fileForm :: Maybe FileInfo -> Html -> MForm s App (FormResult FileInfo, GWidget s App ())
fileForm mv = renderBootstrap $ areq fileField' (fs MsgUploadFilePath) mv

fileField' :: Field s App FileInfo
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
