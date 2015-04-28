{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Base16 as B16
import Cache
import Dataset

main :: IO ()
main = do
  --cache <- mkCache (2^12) "seed"
  cache <- mkCache 512 "seed"
  --putStrLn $ unlines $ map show $ map B16.encode $ V.toList cache
  putStrLn $ show $ B16.encode $ calcDatasetItemBS cache 100

