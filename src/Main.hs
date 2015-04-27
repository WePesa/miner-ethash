
module Main where

import Crypto.Hash
import System.Endian
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as BN
import qualified Data.Binary.Strict.IncrementalGet as IG
import Cache
import Dataset
import qualified Data.Vector as V

main :: IO ()
main = do
  --cache <- mkCache (2^12) (C.pack $ "seed")
  cache <- mkCache 512 (C.pack $ "seed")
  putStrLn $ unlines $ map show $ map B16.encode $ V.toList cache
  --print $ calcDatasetItemBS cache 100

