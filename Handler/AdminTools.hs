module Handler.AdminTools where

import Import
import Data.Maybe (isJust)
import Text.Julius (rawJS)

importForm :: Maybe FileInfo -> Html -> MForm App App (FormResult FileInfo, Widget)
importForm mv fragment = do
  (res, view) <- mreq fileField' fs mv
  liftIO $ putStrLn "[TODO] import csv file!"
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

getAdminToolsR :: Handler RepHtml
getAdminToolsR = do
  (modal1, modal2, modal3) <- (,,) <$> newIdent <*> newIdent <*> newIdent
  (menu1, menu2, menu3) <- (,,) <$> newIdent <*> newIdent <*> newIdent
  (w, e) <- generateFormPost $ importForm Nothing
  tabIs <- fmap (maybe ("maint-user"==) (==)) $ lookupGetParam "tab"
  let photos = [ (img_avatar_avatar_jpg, "User 1"::Text)
               , (img_avatar_avatar2_jpg, "User 2")
               , (img_avatar_avatar3_jpg, "User 3")
               , (img_avatar_avatar4_jpg, "User 4")
               , (img_avatar_avatar5_jpg, "User 5")
               , (img_avatar_avatar6_jpg, "User 6")
               , (img_avatar_avatar7_jpg, "User 7")
               , (img_avatar_avatar8_jpg, "User 8")
               , (img_avatar_avatar9_jpg, "User 9")
               ]
  defaultLayout $ do
    let userlist = $(widgetFile "user-list")
        edituser = $(widgetFile "edit-user")
        killuser = $(widgetFile "kill-user")
        importcsv = $(widgetFile "import-users-csv")
        clientlist = $(widgetFile "client-list")
        editclient = $(widgetFile "edit-client")
    setTitle "Administrator's Tools"
    $(widgetFile "admin-tools")
