module Owl.Helpers.Util 
       ( newIdent2
       , newIdent3
       , newIdent4
       , getCurrentRoute'
       , toGravatarHash
       , gravatarUrl
       , fromLazy
       , toLazy
         -- RSA
       , genKey
       , encrypt
       , decrypt
       , sign
       , verify
       ) where

import Prelude
import Yesod
import qualified Codec.Crypto.RSA as RSA
import Control.Applicative ((<$>),(<*>))
import Control.Arrow (first)
import Crypto.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Char (toLower, isSpace)
import Data.Digest.Pure.MD5 (md5)
import Data.Text (Text)
import qualified Data.Text as T

newIdent2 :: Yesod m => GHandler s m (Text, Text)
newIdent2 = (,) <$> newIdent <*> newIdent

newIdent3 :: Yesod m => GHandler s m (Text, Text, Text)
newIdent3 = (,,) <$> newIdent <*> newIdent <*> newIdent

newIdent4 :: Yesod m => GHandler s m (Text, Text, Text, Text)
newIdent4 = (,,,) <$> newIdent <*> newIdent <*> newIdent <*> newIdent


getCurrentRoute' :: Yesod m => GHandler s m (Maybe (Route m))
getCurrentRoute' = do
  mcr' <- getCurrentRoute
  toMaster <- getRouteToMaster
  return $ fmap toMaster mcr'

toGravatarHash :: Text -> Text
toGravatarHash = T.pack . show . md5 . BL.fromString . map toLower . trim . T.unpack
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

gravatarUrl :: Int -> Text -> Text
gravatarUrl s h = T.concat [ "https://secure.gravatar.com/avatar/"
                           , h
                           , "?d=identicon&s="
                           , T.pack $ show s
                           ]

-- |
--
-- RSA utility
--
fromLazy :: BL.ByteString -> BS.ByteString
fromLazy = BS.pack . BL.unpack
toLazy :: BS.ByteString -> BL.ByteString
toLazy = BL.pack . BS.unpack

genKey :: IO (RSA.PublicKey, RSA.PrivateKey)
genKey = (newGenIO::IO SystemRandom) >>= return . fs . flip RSA.generateKeyPair 1024
  where
    fs (f, s, _) = (f, s)

encode :: BL.ByteString -> BL.ByteString
encode = toLazy . Base64.encode . fromLazy
decode :: BL.ByteString -> BL.ByteString
decode = either BL.pack toLazy . Base64.decode . fromLazy

encrypt :: RSA.PublicKey -> BL.ByteString -> IO (BL.ByteString, SystemRandom)
encrypt pub plain = do
  g <- newGenIO :: IO SystemRandom
  return $ first encode $ RSA.encrypt g pub plain

decrypt :: RSA.PrivateKey -> BS.ByteString -> BS.ByteString
decrypt priv cipher = either BS.pack (fromLazy . RSA.decrypt priv . toLazy) $ Base64.decode cipher

sign :: RSA.PrivateKey -> BL.ByteString -> BL.ByteString
sign = (encode.).RSA.sign

verify :: RSA.PublicKey -> BL.ByteString -> BL.ByteString -> Bool
verify pub plain cipher = RSA.verify pub plain $ decode cipher
