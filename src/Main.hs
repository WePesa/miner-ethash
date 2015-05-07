{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.ByteString.Internal
import qualified Data.ByteString.Base16 as B16
import Data.Word
import qualified Data.Vector as V
import Numeric

import TimeIt

import Cache
import Constants
import Dataset
import Hashimoto

encodeWord8::Word8->String
encodeWord8 c | c < 0x20 || c > 0x7e = "\\x" ++ showHex c ""
encodeWord8 c = [w2c c]

encodeByteString::B.ByteString->String
encodeByteString = (encodeWord8 =<<) . B.unpack

main :: IO ()
main = do
  cache <- mkCache (fromIntegral $ cacheSize 0) "seed"
--  let dataset = calcDataset (fullSize 0) cache
  timeIt $ do
    let (mixDigest, result) =
--        hashimoto (B.pack [1,2,3,4]) (B.pack [1,2,3,4]) 512 ((dataset V.!) . fromIntegral)
          hashimoto (B.pack [1,2,3,4]) (B.pack [1,2,3,4]) (fromIntegral $ fullSize 0) (calcDatasetItem cache . fromIntegral)
    putStrLn $ "mixDigest: " ++ encodeByteString mixDigest
    putStrLn $ "result: " ++ encodeByteString result
