module Handler.Service
       ( postAuthenticateR
       ) where

import Import hiding (object)
import Owl.Helpers.Auth.HashDB (validateUser)

import Control.Monad (mzero)
import Data.Conduit as C
import Network.Wai
import Data.Aeson
import Data.Attoparsec (parse, maybeResult)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Data.HashMap.Strict as M (toList)
import Owl.Helpers.Util
import qualified Settings

-- for Request
data AuthReq = AuthReq
               { ident :: Text
               , pass :: Text
               }
             deriving (Show, Read, Eq)

instance FromJSON AuthReq where
  parseJSON (Object v) = AuthReq <$> v .: "ident" <*> v .: "pass"
  parseJSON _ = mzero
instance ToJSON AuthReq where
  toJSON (AuthReq i p) = object ["ident" .= i, "pass" .= p]

-- for Response
data AuthRes' = AuthRes' { cipher :: LB.ByteString }
instance FromJSON AuthRes' where
  parseJSON (Object o) = AuthRes' <$> o .: "cipher"
  parseJSON _ = mzero
instance ToJSON AuthRes' where
  toJSON (AuthRes' e) = object [ "cipher" .= e ]

data AuthRes = Rejected
               { rejected_ident :: Text
               , rejected_pass :: Text
               , rejected_reason :: Text
               }
             | Accepted
               { accepted_ident :: Text
               , accepted_email :: Maybe Text
               }
             deriving (Show, Read, Eq)

instance FromJSON AuthRes where
  parseJSON (Object o) = case M.toList o of
    [("rejected", Object o')] ->
      Rejected <$> o' .: "ident" <*> o' .: "pass" <*> o' .: "reason"
    [("accepted", Object o')] ->
      Accepted <$> o' .: "ident" <*> o' .:? "email"
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON AuthRes where
  toJSON (Rejected i p r) = object [ "rejected" .= object [ "ident" .= i
                                                          , "pass" .= p
                                                          , "reason" .= r
                                                          ]
                                   ]
  toJSON (Accepted i me) = object [ "accepted" .= object [ "ident" .= i
                                                         , "email" .= me
                                                         ]
                                  ]

postAuthenticateR :: Handler RepJson
postAuthenticateR = do
  req <- getRequest
  let (req', h) = (reqWaiRequest req, requestHeaders req')
  mc <- liftIO $ runResourceT $ requestBody req' $$ await
  let mchecked = mc >>= \cipher ->
        lookup "X-Owl-clientId" h >>= \clientId ->
        lookup "X-Owl-signature" h >>= \signature ->
        lookup clientId Settings.clientPublicKeys >>= \pubkey ->
        return (verify pubkey cipher signature,
                pubkey,
                maybeResult $ parse json $ decrypt Settings.owl_priv cipher
               )
  case mchecked of
    Just (True, key, Just v) -> case fromJSON v of
      Success (AuthReq ident pass) ->
        jsonToRepJson =<< authentication key (ident, pass)
      Error msg -> invalidArgs [T.pack msg]
    _ -> permissionDeniedI MsgYouUnauthorizedClient
  where
    authentication key (ident, pass) = do
      render <- getMessageRender
      checked <- validateUser (UniqueUser ident) pass
      r <- if checked
        then do
        u <- runDB $ getBy404 (UniqueUser ident)
        return $ case userVerstatus (entityVal u) of
              Just Verified -> Accepted ident (userEmail (entityVal u))
              Just Unverified ->
                Rejected ident pass $ render MsgUnverifiedEmailaddress
              Nothing ->
                Rejected ident pass $ render MsgUnverifiedEmailaddress
        else do
        return $ Rejected ident pass $ render MsgTheAccountPasswordInvalid
      return . AuthRes' . fst =<< (liftIO $ encrypt key $ encode r)
