module Handler.Service.API.Internal.Res where

import Import hiding (object)
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB

data OwlRes = OwlRes { cipher :: LB.ByteString }
instance FromJSON OwlRes where
  parseJSON (Object o) = OwlRes <$> o .: "cipher"
  parseJSON _ = mzero
instance ToJSON OwlRes where
  toJSON (OwlRes e) = object [ "cipher" .= e ]

