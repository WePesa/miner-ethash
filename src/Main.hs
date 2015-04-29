{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.Vector as V

import Cache
import Dataset
import Hashimoto

main :: IO ()
main = do
  --cache <- mkCache (2^12) "seed"
  cache <- mkCache 512 "seed"
  --putStrLn $ unlines $ map show $ map B16.encode $ V.toList cache
  --putStrLn $ show $ B16.encode $ calcDatasetItem cache 100
  let dataset = calcDataset 512 cache
  putStrLn $ show $ map B16.encode $ V.toList dataset
  --putStrLn $ show $ hashimoto (B.pack [1,2,3,4]) (B.pack [1,2,3,4]) 512 dataset
