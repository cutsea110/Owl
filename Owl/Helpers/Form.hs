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

fs' :: RenderMessage m msg => msg -> [(Text, Text)] -> FieldSettings m
fs' msg attrs = FieldSettings { fsLabel = SomeMessage msg
                              , fsTooltip = Nothing
                              , fsId = Nothing
                              , fsName = Nothing
                              , fsAttrs = attrs
                              }


accountForm :: Maybe Text -> Html -> MForm s App (FormResult Text, GWidget s App ())
accountForm mv = renderBootstrap $ areq textField (fs MsgAccountID) mv

accountPasswordForm :: Maybe (Text, Text) -> Html -> MForm s App (FormResult (Text, Text), GWidget s App ())
accountPasswordForm mv = renderBootstrap $ (,)
                         <$> areq textField (fs MsgAccountID) (fst <$> mv)
                         <*> areq passwordConfirmField "" (snd <$> mv)

passwordForm :: User -> Maybe (Text, Text) -> Form Text
passwordForm u mv fragment = do
  (res, widget) <- flip renderBootstrap fragment $ (,)
              <$> areq passwordField (fs MsgCurrentPassword) (fst <$> mv)
              <*> areq passwordConfirmField "" (snd <$> mv)
  res' <- lift $ case res of
    FormSuccess (curPass, newPass) -> do
      checkPass <- validateUser (mkUnique u) curPass
      return $ if checkPass then FormSuccess newPass else FormFailure [msg]
    _ -> return $ snd <$> res
  return (res', widget)
  where
    mkUnique = UniqueUser . userUsername
    msg = "Invalid current password"

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
emailForm mv = renderBootstrap $
               areq emailField (fs' MsgEmail [("placeholder", "your@example.com")]) mv

userEmailForm :: Maybe (Maybe Text, Maybe VerStatus, Maybe Text) -> Html -> MForm s App (FormResult (Maybe Text, Maybe VerStatus, Maybe Text), GWidget s App ())
userEmailForm mv = renderBootstrap $ (,,)
                   <$> aopt emailField (fs' MsgEmail [("placeholder", "your@example.com")]) (fst3 <$> mv)
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
