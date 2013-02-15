module Owl.Helpers.Form 
       ( accountForm
       , newAccountForm
       , passwordForm
       , passwordForm'
       , emailForm
       , emailForm'
       , verifyForm
       , profileForm
       , profileForm'
       , fileForm
       ) where

import Import
import Control.Arrow ((&&&))
import qualified Data.Text as T (pack)
import Owl.Helpers.Auth.HashDB (validateUser)
import Owl.Helpers.Util
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


accountForm :: Maybe Text -> Form Text
accountForm mv = renderBootstrap $ areq textField (fs MsgAccountID) mv

newAccountForm :: Maybe (Text, Role, Text, Text) -> Form (Text, Role, Text)
newAccountForm mv fragment = do
  (y, l) <- lift $ (,) <$> getYesod <*> fmap reqLangs getRequest
  (res, widget) <- flip renderBootstrap fragment $ (,,,)
                   <$> areq textField (fs MsgAccountID) (fst4 <$> mv)
                   <*> areq (selectFieldList rls) (fs MsgRole) (snd4 <$> mv)
                   <*> areq passwordField (fs MsgNewPassword) (thd4 <$> mv)
                   <*> areq passwordField (fs MsgConfirmNewPassword) (frh4 <$> mv)
  return $ case res of
    FormSuccess (id', role, newPass, newPass')
      | newPass == newPass' -> (FormSuccess (id', role, newPass), widget)
      | otherwise -> (FormFailure [renderMessage y l MsgPasswordsUnmatch], widget)
    _ -> (drop4th <$> res, widget)
  where
    rls :: [(Text, Role)]
    rls = map ((T.pack . show) &&& id) [minBound..maxBound]
    drop4th (f, s, t, _) = (f, s, t)
    
passwordForm' :: User -> Maybe (Text, Text, Text) -> Form Text
passwordForm' u mv fragment = do
  (y, l) <- lift $ (,) <$> getYesod <*> fmap reqLangs getRequest
  (res, widget) <- flip renderBootstrap fragment $ (,,)
                   <$> areq passwordField (fs MsgCurrentPassword) (fst3 <$> mv)
                   <*> areq passwordField (fs MsgNewPassword) (snd3 <$> mv)
                   <*> areq passwordField (fs MsgConfirmNewPassword) (thd3 <$> mv)
  lift $ case res of
    FormSuccess (curPass, newPass, newPass')
      | newPass == newPass' -> do
        checkPass <- validateUser (UniqueUser $ userUsername u) curPass
        if checkPass 
          then return (FormSuccess newPass, widget) 
          else return (FormFailure [renderMessage y l MsgInvalidCurrentPassword], widget)
      | otherwise -> return (FormFailure [renderMessage y l MsgPasswordsUnmatch], widget)
    _ -> return (snd3 <$> res, widget)

passwordForm :: Maybe (Text, Text) -> Form Text
passwordForm mv fragment = do
  (y, l) <- lift $ (,) <$> getYesod <*> fmap reqLangs getRequest
  (res, widget) <- flip renderBootstrap fragment $ (,)
                   <$> areq passwordField (fs MsgNewPassword) (fst <$> mv)
                   <*> areq passwordField (fs MsgConfirmNewPassword) (snd <$> mv)
  return $ case res of
    FormSuccess (newPass, newPass') 
      | newPass == newPass' -> (FormSuccess newPass, widget)
      | otherwise -> (FormFailure [renderMessage y l MsgPasswordsUnmatch], widget)
    _ -> (fst <$> res, widget)

emailForm :: Maybe Text -> Form Text
emailForm mv = renderBootstrap $
               areq emailField (fs' MsgEmail [("placeholder", "your@example.com")]) mv

emailForm' :: Maybe (Maybe Text, Maybe VerStatus, Maybe Text) -> Form (Maybe Text, Maybe VerStatus, Maybe Text)
emailForm' mv = renderBootstrap $ (,,)
                   <$> aopt emailField (fs' MsgEmail [("placeholder", "your@example.com")]) (fst3 <$> mv)
                   <*> aopt (selectFieldList vss) (fs MsgVerstatus) (snd3 <$> mv)
                   <*> aopt textField (fs MsgVerkey) (thd3 <$> mv)
  where
    vss :: [(Text, VerStatus)]
    vss = map ((T.pack . show) &&& id) [minBound..maxBound]

verifyForm :: Maybe Text -> Form Text
verifyForm mv = renderBootstrap $ areq hiddenField "verkey" mv

profileForm :: Maybe (Text, Text, Maybe Textarea) -> Form (Text, Text, Maybe Textarea)
profileForm mv = renderBootstrap $ (,,)
                 <$> areq textField (fs MsgFamilyName) (fst3 <$> mv)
                 <*> areq textField (fs MsgGivenName) (snd3 <$> mv)
                 <*> aopt textareaField (fs MsgProfile) (thd3 <$> mv)

profileForm' :: Maybe (Text, Text, Role, Maybe Textarea) -> Form (Text, Text, Role, Maybe Textarea)
profileForm' mv = renderBootstrap $ (,,,)
                  <$> areq textField (fs MsgFamilyName) (fst4 <$> mv)
                  <*> areq textField (fs MsgGivenName) (snd4 <$> mv)
                  <*> areq (selectFieldList rls) (fs MsgRole) (thd4 <$> mv)
                  <*> aopt textareaField (fs MsgProfile) (frh4 <$> mv)
  where
    rls :: [(Text, Role)]
    rls = map ((T.pack . show) &&& id) [minBound..maxBound]


fileForm :: Maybe FileInfo -> Form FileInfo
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
