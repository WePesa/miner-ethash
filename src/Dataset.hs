{-# LANGUAGE TupleSections #-}

module Dataset (
  Slice,
  calcDatasetItem
--  calcDataset
  ) where

import Control.Monad
import qualified Crypto.Hash.SHA3 as SHA3
import Constants
import qualified Data.Array.Unboxed as A
import qualified Data.Array.IO as MA
import Data.Bits
import Data.Word

import Cache
import Util

--import Debug.Trace

type Slice = MA.IOUArray Word32 Word32

copySlice::Cache->Word32->IO Slice
copySlice cache i = do
  x <- MA.newArray (0,15) 0
  forM_ [0..15] $ \k ->
    MA.writeArray x k (cache A.! (i,k))
  return x

calcDatasetItem::Cache->Word32->IO Slice
calcDatasetItem cache i = do
  let n = getCacheWidth cache

  mix <- copySlice cache (i `mod` n)

  MA.writeArray mix 0 =<< (fmap (xor i) $ MA.readArray mix 0)

  mixBytes' <- sequence $ map (MA.readArray mix) [0..15]
  let theHash = shatter $ SHA3.hash 512 $ repair mixBytes'
  sequence_ $ map (uncurry $ MA.writeArray mix) $ zip [0..] theHash

  forM_ [0..fromIntegral $ datasetParents-1] $ \j ->
    cacheFunc cache i j mix

  mixBytes'' <- sequence $ map (MA.readArray mix) [0..15]
  let theHash' = shatter $ SHA3.hash 512 $ repair mixBytes''
  sequence_ $ map (uncurry $ MA.writeArray mix) $ zip [0..] theHash'

  return mix



cacheFunc::Cache->Word32->Word32->Slice->IO ()
cacheFunc cache i j mix = do
 let r = fromInteger $ hashBytes `div` wordBytes
     n = getCacheWidth cache

 cacheIndex <- fmap (fnv (i `xor` j)) $ MA.readArray mix $ j `mod` r

 forM_ [0..15] $ \k -> do
   v1 <- MA.readArray mix k
   let v2 = cache A.! (cacheIndex `mod` n, k)
   MA.writeArray mix k $ fnv v1 v2

{-
calcDataset::Word32->Cache->Cache
calcDataset size cache =
  A.listArray ((0,0), ((size-1) `div` fromInteger hashBytes, 16))
  $ concatMap (A.elems . calcDatasetItem cache)
  [0..]
-}
