
module Dataset (
  calcDatasetItemBS,
  cacheFunc
  ) where

import Control.Monad
import qualified Crypto.Hash.SHA3 as SHA3
import Constants
import qualified Data.Binary.Get as G
import Data.Binary.Put
import qualified Data.Binary.Strict.IncrementalGet as IG
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Bits
import qualified Data.Vector as V
import Data.Word

import Util

--import Debug.Trace

fnvPrime::Word32
fnvPrime = 16777619

fnv::Word32->Word32->Word32
fnv v1 v2 = v1 * fnvPrime `xor` v2

{-
def calc_dataset_item(cache, i):
    n = len(cache)
    r = HASH_BYTES // WORD_BYTES
    # initialize the mix
    mix = copy.copy(cache[i % n])
    mix[0] ^= i
    mix = sha3_512(mix)
    # fnv it with a lot of random cache nodes based on i                                                                                                                                                    
    for j in range(DATASET_PARENTS):
        cache_index = fnv(i ^ j, mix[j % r])
        mix = map(fnv, mix, cache[cache_index % n])
    return sha3_512(mix)
-}

    
calcDatasetItemBS :: V.Vector B.ByteString -> Word32 -> B.ByteString
calcDatasetItemBS cache i =
  SHA3.hash 512 $ fst $ iterate (cacheFunc cache i) ( mixInit, 0 ) !! datasetParents
   where mixInit = SHA3.hash 512 $
                       BL.toStrict (runPut (putWord32le i) `BL.append` BL.replicate 60 0)  `xorBS`
                       (cache V.! (fromIntegral i `mod` n))
         n = V.length cache

cacheFunc :: V.Vector B.ByteString -> Word32 -> ( B.ByteString, Word32 ) -> (B.ByteString, Word32)
cacheFunc cache i (mix, j) =
  (lint2BS $ zipWith fnv mixLst mixWithLst, j+1)
  where (IG.Finished _ mixLst) = IG.runGet (replicateM 16 IG.getWord32le) mix
        (IG.Finished _ mixWithLst) = IG.runGet (replicateM 16 IG.getWord32le) (cache V.! fromIntegral ( cacheIndex  `mod` n))
        cacheIndex = fnv (fromIntegral i `xor` j) (mixLst !! fromIntegral (j `mod` r))
        r = fromInteger $ hashBytes `div` wordBytes ::Word32
        n = fromIntegral $ V.length cache

lint2BS :: [Word32] -> B.ByteString
lint2BS lst = B.concat $ fmap (BL.toStrict . runPut . putWord32le) lst

