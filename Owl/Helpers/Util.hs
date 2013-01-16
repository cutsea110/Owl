module Owl.Helpers.Util 
       ( newIdent2
       , newIdent3
       , newIdent4
       , getCurrentRoute'
       , toGravatarHash
       , gravatarUrl
       ) where

import Prelude (($), (.), return, fmap, Maybe, show, Int, dropWhile, reverse, map)
import Yesod
import Control.Applicative ((<$>),(<*>))
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
