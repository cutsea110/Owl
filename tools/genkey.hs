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

fromLazy :: BL.ByteString -> BS.ByteString
fromLazy = BS.pack . BL.unpack
toLazy :: BS.ByteString -> BL.ByteString
toLazy = BL.pack . BS.unpack

genKey :: IO (RSA.PublicKey, RSA.PrivateKey)
genKey = (newGenIO::IO SystemRandom) >>= return . fs . flip RSA.generateKeyPair 2048
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

decrypt :: RSA.PrivateKey -> BL.ByteString -> BL.ByteString
decrypt priv cipher = either BL.pack (RSA.decrypt priv . toLazy) $ Base64.decode $ fromLazy cipher

sign :: RSA.PrivateKey -> BL.ByteString -> BL.ByteString
sign = (encode.).RSA.sign

verify :: RSA.PublicKey -> BL.ByteString -> BL.ByteString -> Bool
verify pub plain cipher = RSA.verify pub plain $ decode cipher

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
