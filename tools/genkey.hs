{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow (first)
import Crypto.Random
import qualified Codec.Crypto.RSA as RSA
import qualified Crypto.PubKey.OpenSsh as SSH
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import qualified Data.ByteString.Base64 as Base64
import System.Environment (getArgs)
import Yesod.Auth.Owl.Util

main :: IO ()
main = do
  args <- getArgs
  (pub,priv) <- genKey
  let email = if null args then "your@example.com" else head args
  let epk = SSH.encode (SSH.OpenSshPublicKeyRsa pub $ BS.pack email)
  putStr "encoded public-key: "
  BS.putStrLn epk
  
  putStr "plain public-key: "
  putStrLn $ show pub
  
  putStr "plain private-key: "
  putStrLn $ show priv
