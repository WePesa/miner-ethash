{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.ByteString.Internal
--import qualified Data.ByteString.Base16 as B16
import Data.Word
--import qualified Data.Vector as V
import Numeric

import Cache
import Dataset
import Hashimoto

encodeWord8::Word8->String
encodeWord8 c | c < 0x20 || c > 0x7e = "\\x" ++ showHex c ""
encodeWord8 c = [w2c c]

encodeByteString::B.ByteString->String
encodeByteString = (encodeWord8 =<<) . B.unpack

main :: IO ()
main = do
  --cache <- mkCache (2^12) "seed"
  cache <- mkCache 512 "seed"
  --putStrLn $ unlines $ map show $ map B16.encode $ V.toList cache
  --putStrLn $ show $ B16.encode $ calcDatasetItem cache 100
  let dataset = calcDataset 512 cache
  --putStrLn $ show $ map B16.encode $ V.toList dataset
  let (mixDigest, result) = hashimoto [1,2,3,4] [1,2,3,4] 512 dataset
  putStrLn $ "mixDigest: " ++ encodeByteString mixDigest
  putStrLn $ "result: " ++ encodeByteString result
