module Handler.Service
       ( postAuthenticateR
       , postChangePasswordR
       ) where

import Import hiding (object)
import Owl.Helpers.Auth.HashDB (validateUser, setPassword)
import Yesod.Auth.Owl.Util
import Yesod.Auth.Owl.Auth as A
import Yesod.Auth.Owl.ChangePass as CP

import Crypto.PubKey.RSA (PublicKey(..))
import Network.Wai
import Data.Aeson
import Data.Attoparsec (parse, maybeResult)
import Data.Conduit (($$), await)
import Data.List (find)
import qualified Data.Text as T
import qualified Settings
import qualified Data.ByteString.Char8 as BS

verifyRequest :: Handler (Maybe (Bool, PublicKey, Maybe Value))
verifyRequest = do
  req <- getRequest
  let (req', h) = (reqWaiRequest req, requestHeaders req')
  mcipher <- liftIO $ requestBody req' $$ await
  maybe (return Nothing) checker $
    (,,) <$> mcipher <*> lookup "X-Owl-clientId" h <*> lookup "X-Owl-signature" h
  where
    checker :: (BS.ByteString, BS.ByteString, BS.ByteString) -> Handler (Maybe (Bool, PublicKey, Maybe Value))
    checker (cph, cid, sig) =
      return $ find ((==cid).clientId) Settings.clientPublicKeys >>= \c ->
        return (verify (pubkey c) (toLazy cph) (toLazy sig),
                pubkey c,
                maybeResult $ parse json $ decrypt Settings.owl_priv cph
               )

postAuthenticateR :: Handler Value
postAuthenticateR = do
  mchecked <- verifyRequest
  case mchecked of
    Just (True, key, Just v) -> case fromJSON v of
      Success (AuthReq i p) ->
        returnJson =<< authentication key (i, p)
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
                A.Accepted i (userEmail (entityVal u))
              Just Unverified ->
                A.Rejected i p $ render MsgUnverifiedEmailaddress
              Nothing ->
                A.Rejected i p $ render MsgUnverifiedEmailaddress
        else do
        return $ A.Rejected i p $ render MsgTheAccountPasswordInvalid
      return . OwlRes . fst =<< (liftIO $ encrypt key $ encode r)

postChangePasswordR :: Handler Value
postChangePasswordR = do
  mchecked <- verifyRequest
  case mchecked of
    Just (True, key, Just v) -> case fromJSON v of
      Success (ChangePassReq i c p p2) ->
        returnJson =<< changePass key (i, c, p, p2)
      Error msg -> invalidArgs [T.pack msg]
    _ -> permissionDeniedI MsgYouUnauthorizedClient
  where
    changePass key (i, c, p, p2) = do
      render <- getMessageRender
      checked <- validateUser (UniqueUser i) c
      r <- if checked
           then if p == p2
                then do
                  runDB $ do
                    u <- getBy404 $ UniqueUser i
                    replace (entityKey u) =<< setPassword p (entityVal u)
                  return $ CP.Accepted i p
                else do
                  return $ CP.Rejected i c p p2 $ render MsgPasswordsUnmatch
           else do
             return $ CP.Rejected i c p p2 $ render MsgTheAccountPasswordInvalid
      return . OwlRes . fst =<< (liftIO $ encrypt key $ encode r)
