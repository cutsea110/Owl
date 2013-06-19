{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import qualified Crypto.PubKey.OpenSsh as SSH
import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs)
import Yesod.Auth.Owl.Util

main :: IO ()
main = do
  args <- getArgs
  (pub,priv) <- genKey
  let email = if null args then "your@example.com" else head args
  let epk = SSH.encodePublic (SSH.OpenSshPublicKeyRsa pub $ BS.pack email)
  putStr "encoded public-key: "
  BS.putStrLn epk
  
  putStr "plain public-key: "
  putStrLn $ show pub
  
  putStr "plain private-key: "
  putStrLn $ show priv
