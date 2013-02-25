module Handler.Service
       ( postAuthenticateR
       ) where

import Import hiding (object)
import Owl.Helpers.Auth.HashDB (validateUser)
import Handler.Service.API.Auth as A
import Handler.Service.API.ChangePass as CP

import Crypto.PubKey.RSA (PublicKey(..))
import Data.Conduit as C
import Network.Wai
import Data.Aeson
import Data.Attoparsec (parse, maybeResult)
import Data.List (find)
import qualified Data.Text as T
import Owl.Helpers.Util
import qualified Settings
import qualified Data.ByteString.Char8 as BS

verifyRequest :: Handler (Maybe (Bool, PublicKey, Maybe Value))
verifyRequest = do
  req <- getRequest
  let (req', h) = (reqWaiRequest req, requestHeaders req')
  mcipher <- liftIO $ runResourceT $ requestBody req' $$ await
  maybe (return Nothing) checker $
    (,,) <$> mcipher <*> lookup "X-Owl-clientId" h <*> lookup "X-Owl-signature" h
  where
    checker :: (BS.ByteString, BS.ByteString, BS.ByteString) -> Handler (Maybe (Bool, PublicKey, Maybe Value))
    checker (cph, cid, sig) =
      return $ find ((==cid).clientId) Settings.clientPublicKeys >>= \c ->
        return (verify (pubkey c) cph sig,
                pubkey c,
                maybeResult $ parse json $ decrypt Settings.owl_priv cph
               )

postAuthenticateR :: Handler RepJson
postAuthenticateR = do
  mchecked <- verifyRequest
  case mchecked of
    Just (True, key, Just v) -> case fromJSON v of
      Success (AuthReq i p) ->
        jsonToRepJson =<< authentication key (i, p)
      Error msg -> invalidArgs [T.pack msg]
    _ -> permissionDeniedI MsgYouUnauthorizedClient
  where
    authentication key (i, p) = do
      render <- getMessageRender
      checked <- validateUser (UniqueUser i) p
      r <- if checked
        then do
        u <- runDB $ getBy404 (UniqueUser i)
        return $ case userVerstatus (entityVal u) of
              Just Verified ->
                Accepted i (userEmail (entityVal u))
              Just Unverified ->
                Rejected i p $ render MsgUnverifiedEmailaddress
              Nothing ->
                Rejected i p $ render MsgUnverifiedEmailaddress
        else do
        return $ Rejected i p $ render MsgTheAccountPasswordInvalid
      return . OwlRes . fst =<< (liftIO $ encrypt key $ encode r)
