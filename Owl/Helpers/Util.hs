module Owl.Helpers.Util
       ( newIdent2
       , newIdent3
       , newIdent4
       , getCurrentRoute'
       , toGravatarHash
       , gravatarUrl
       , showSshKey
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
import qualified Crypto.PubKey.OpenSsh as SSH (OpenSshPublicKey(..), encodePublic, encodePrivate)
import Database.Persist.Sql
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Char (toLower, isSpace)
import Data.Digest.Pure.MD5 (md5)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.HT (fst3, snd3, thd3)

newIdent2 :: Yesod m => HandlerT m IO (Text, Text)
newIdent2 = (,) <$> newIdent <*> newIdent

newIdent3 :: Yesod m => HandlerT m IO (Text, Text, Text)
newIdent3 = (,,) <$> newIdent <*> newIdent <*> newIdent

newIdent4 :: Yesod m => HandlerT m IO (Text, Text, Text, Text)
newIdent4 = (,,,) <$> newIdent <*> newIdent <*> newIdent <*> newIdent

{-# DEPRECATED getCurrentRoute' "Use original getCurrentRoute" #-}
getCurrentRoute' :: Yesod m => HandlerT m IO (Maybe (Route m))
getCurrentRoute' = getCurrentRoute

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

showSshKey :: RSA.PublicKey -> BS.ByteString
showSshKey pub = SSH.encodePublic $ SSH.OpenSshPublicKeyRsa pub ""

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
    maxpage = ceiling (fromIntegral i / fromIntegral n) - 1
    ints = mkPagenate f w maxpage c
    ps = map (map make) ints
    toText = T.pack . show
    make n | n /= c    = Right (toText (n+1), (rt, ("p", toText n):qs))
           | otherwise = Left (toText (n+1))

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
