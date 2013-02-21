module Handler.Service.API.ChangePass where

import Import hiding (object)
import Control.Monad (mzero)
import Data.Aeson

-- Request for Change Password API
data ChangePassReq = ChangePassReq
                     { ident :: Text
                     , current_pass :: Text
                     , new_pass :: Text
                     , new_pass2 :: Text
                     }
                   deriving (Show, Read, Eq)

instance FromJSON ChangePassReq where
  parseJSON (Object v) = ChangePassReq
                         <$> v .: "ident"
                         <*> v .: "current_pass"
                         <*> v .: "new_pass"
                         <*> v .: "new_pass2"
  parseJSON _ = mzero
instance ToJSON ChangePassReq where
  toJSON (ChangePassReq i c n n2) = object [ "ident" .= i
                                           , "current_pass" .= c
                                           , "new_pass" .= n
                                           , "new_pass2" .= n2
                                           ]
