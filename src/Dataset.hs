
module Dataset (
  calcDatasetItem,
  calcDataset
  ) where

import Control.Monad
import qualified Crypto.Hash.SHA3 as SHA3
import Constants
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Bits
import qualified Data.Vector as V
import Data.Word

import Cache
import Util

--import Debug.Trace

fnvPrime::Word32
fnvPrime = 16777619

fnv::Word32->Word32->Word32
fnv v1 v2 = v1 * fnvPrime `xor` v2
    
calcDatasetItem::Cache->Word32->B.ByteString
calcDatasetItem cache i =
  SHA3.hash 512 $ fst $ iterate (cacheFunc cache i) ( mixInit, 0 ) !! datasetParents
   where mixInit = SHA3.hash 512 $
                       BL.toStrict (runPut (putWord32le i) `BL.append` BL.replicate 60 0)  `xorBS`
                       (cache V.! (fromIntegral i `mod` n))
         n = V.length cache

cacheFunc :: V.Vector B.ByteString -> Word32 -> ( B.ByteString, Word32 ) -> (B.ByteString, Word32)
cacheFunc cache i (mix, j) =
  (repair $ zipWith fnv mixLst mixWithLst, j+1)
  where mixLst = shatter mix
        mixWithLst = shatter (cache V.! fromIntegral ( cacheIndex  `mod` n))
        cacheIndex = fnv (fromIntegral i `xor` j) (mixLst !! fromIntegral (j `mod` r))
        r = fromInteger $ hashBytes `div` wordBytes
        n = fromIntegral $ V.length cache

shatter::B.ByteString->[Word32]
shatter = runGet (replicateM 16 getWord32le) . BL.fromStrict

repair::[Word32]->B.ByteString
repair = B.concat . fmap (BL.toStrict . runPut . putWord32le) 

calcDataset::Word32->Cache->V.Vector B.ByteString
calcDataset size cache =
  V.fromList $ map (calcDatasetItem cache)
                   [0..(size-1) `div` fromInteger hashBytes]
