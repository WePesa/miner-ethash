{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.ByteString.Internal
import Data.Word
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


  let fullSize' = fromIntegral $ fullSize 0
      --getItem = (dataset V.!) . fromIntegral
      getItem = calcDatasetItem cache . fromIntegral
      block = B.pack [1,2,3,4]
      nonce = B.pack [1,2,3,4]

  timeIt $ do
    (mixDigest, result) <- hashimoto block nonce fullSize' getItem
    putStrLn $ "mixDigest: " ++ encodeByteString mixDigest
    putStrLn $ "result: " ++ encodeByteString result

  timeIt $ do
    (mixDigest, result) <- hashimoto block nonce fullSize' getItem
    putStrLn $ "mixDigest: " ++ encodeByteString mixDigest
    putStrLn $ "result: " ++ encodeByteString result

  timeIt $ do
    (mixDigest, result) <- hashimoto block nonce fullSize' getItem
    putStrLn $ "mixDigest: " ++ encodeByteString mixDigest
    putStrLn $ "result: " ++ encodeByteString result

