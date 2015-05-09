{-# LANGUAGE TupleSections #-}

module Dataset (
  Slice,
  sliceToByteString,
  calcDatasetItem,
  calcDataset
  ) where

import qualified Crypto.Hash.SHA3 as SHA3
import Constants
import qualified Data.Array.Unboxed as A
import qualified Data.ByteString as B
import Data.Bits
import Data.Word

import Cache
import Util

--import Debug.Trace

type Slice = A.UArray Word32 Word32

sliceToByteString::Slice->B.ByteString
sliceToByteString = repair . A.elems

getSlice::Cache->Word32->[Word32]
getSlice cache i = map ((cache A.!) . (i,)) [0..]

calcDatasetItem::Cache->Word32->Slice
calcDatasetItem cache i =
  A.listArray (0, 15) $ shatter $ SHA3.hash 512 $ sliceToByteString $ A.listArray (0, 15) $ fst $ iterate (cacheFunc cache i) (mixInit, 0 ) !! datasetParents
   where mixInit = shatter $ SHA3.hash 512 $
                   sliceToByteString $
                   A.accum xor (A.listArray (0,15) $ getSlice cache (fromIntegral i `mod` n)) [(0,i)]
         n = getCacheWidth cache

cacheFunc::Cache->Word32->([Word32], Word32)->([Word32], Word32)
cacheFunc cache i (mix, j) =
  (zipWith fnv mix (getSlice cache $ cacheIndex `mod` n), j+1)
  where cacheIndex = fnv (fromIntegral i `xor` j) (mix !! fromIntegral (j `mod` r))
        r = fromInteger $ hashBytes `div` wordBytes
        n = getCacheWidth cache

calcDataset::Word32->Cache->Cache
calcDataset size cache =
  A.listArray ((0,0), ((size-1) `div` fromInteger hashBytes, 16))
  $ concatMap (A.elems . calcDatasetItem cache)
  [0..]
