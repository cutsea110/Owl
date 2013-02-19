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
       , fst3
       , snd3
       , thd3
       , fst4
       , snd4
       , thd4
       , frh4
         -- like query
       , ilike
         -- pagenate
       , pagenate
       ) where

import Prelude
import Yesod
import qualified Codec.Crypto.RSA as RSA
import Control.Applicative ((<$>),(<*>))
import Control.Arrow (first)
import Crypto.Random
import Database.Persist.Store
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Char (toLower, isSpace)
import Data.Digest.Pure.MD5 (md5)
import Data.Either
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.HT (fst3, snd3, thd3)

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

verify :: RSA.PublicKey -> BS.ByteString -> BS.ByteString -> Bool
verify pub plain cipher = RSA.verify pub plain' $ decode cipher'
  where
    plain' = toLazy plain
    cipher' = toLazy cipher

fst4 :: (a, b, c, d) -> a
fst4 (f,_,_,_) = f
snd4 :: (a, b, c, d) -> b
snd4 (_,s,_,_) = s
thd4 :: (a, b, c, d) -> c
thd4 (_,_,t,_) = t
frh4 :: (a, b, c, d) -> d
frh4 (_,_,_,f) = f

ilike :: EntityField v Text -> Text -> Filter v
ilike field val = Filter field (Left $ T.concat ["%", escape val, "%"]) (BackendSpecificFilter "ILIKE")
  where
    escape = T.foldr esc ""
    esc c t | T.any (==c) "%?'" = '\\' `T.cons` c `T.cons` t
            | otherwise = c `T.cons` t

pagenate :: Int    -- fillGapWidth
            -> Int -- pagenateWidth
            -> Int -- numPerPage
            -> (Route a, [(Text, Text)]) -- for @?{}
            -> Int -- itemCount
            -> Int -- current page
            -> [Either Text (Text, (Route a, [(Text, Text)]))]
pagenate f w n (rt, qs) i c = concat $ intersperse [Left ".."] $ ps
  where
    maxpage = ceiling (fromIntegral i/fromIntegral n) - 1
    ints = mkPagenate f w maxpage c
    ps = map (map make) ints
    make n | n /= c    = Right (T.pack $ show (n+1), (rt, ("p", T.pack $ show n):qs))
           | otherwise = Left (T.pack $ show (n+1))

mkPagenate :: Int -> Int -> Int -> Int -> [[Int]]
mkPagenate fillGap width maxpage current =
  if leftConnected && rightConnected
  then [[ll..rr]]
  else if leftConnected
       then [[ll..cr], [rl..rr]]
       else if rightConnected
            then [[ll..lr],[cl..rr]]
            else [[ll..lr],[cl..cr],[rl..rr]]
  where
    leftConnected = cl-lr <= fillGap
    rightConnected = rl-cr <= fillGap
    (ll, lr) = (0, width)
    (cl, cr) = (current-width, current+width)
    (rl, rr) = (maxpage-width, maxpage)
