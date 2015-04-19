
module Main where

import Crypto.Hash
import Cache
import qualified Data.ByteString as B

main :: IO ()
main = do
           putStrLn $ "Hello"
  
{-
           let b = B.pack [1,2,3,4]
           putStrLn $ show (hash b :: Digest SHA1)
           putStrLn $ show (hash b :: Digest SHA224)
           putStrLn $ show (hash b :: Digest SHA512)
           putStrLn $ show (hash b :: Digest SHA3_512)
           putStrLn $ show (hash b :: Digest MD5)
 -}        

