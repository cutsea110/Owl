module Handler.Service
       ( postAuthenticateR
       ) where

import Import hiding (object)
import Data.Maybe (fromJust)
import Owl.Helpers.Auth.HashDB (validateUser)

import Control.Monad (mzero, (=<<), (<=<), (>=>))
import Data.Conduit as C
import Network.Wai
import Data.Aeson
import Data.Aeson.Parser (json)
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Text (Text)
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
  r <- fmap reqWaiRequest getRequest
  v <- liftIO $ runResourceT $ decrypt' (requestBody r) $$ sinkParser json
  case fromJSON v of
    Success (AuthReq ident pass) -> jsonToRepJson =<< authentication (ident, pass)
    Error msg -> invalidArgs [T.pack msg]
  where
    decrypt' = mapOutput (decrypt Settings.owl_priv)
    authentication (ident, pass) = do
      checked <- validateUser (UniqueUser ident) pass
      r <- if checked
        then do
        u <- runDB $ getBy404 (UniqueUser ident)
        return $ case userVerstatus (entityVal u) of
              Just Verified -> Accepted ident (userEmail (entityVal u))
              Just Unverified -> Rejected ident pass "Unverified email address"
              Nothing -> Rejected ident pass "Unverified email address"
        else do
        return $ Rejected ident pass "The account/password are invalid"
      return . AuthRes' . fst =<< (liftIO $ encrypt Settings.bisocie_pub $ encode r)
