
module Dataset
       ( calcDatasetItemBS,
       )
    where

import Control.Monad
import Control.Monad.State
import Control.Monad.Primitive
import Crypto.Hash
import Constants
import Data.List
import qualified Data.Binary as BN
import qualified Data.Binary.Get as G
import qualified Data.Binary.Strict.IncrementalGet as IG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base64 as B
import Data.Byteable
import Data.Bits
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Repr.Vector as RV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import System.Endian
import Cache

    
fnvPrime = 16777619

fnv :: BN.Word64 -> BN.Word64 -> BN.Word64 
fnv v1 v2 = (v1 * (fnvPrime ^v2)) `mod` (2^32)

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

-- assume bs is 512 bits - unsafe
    
bs2LW64 :: IG.Get [ BN.Word64 ] [ BN.Word64 ]
bs2LW64  = do
  replicateM 8 $ do
     b <- IG.getWord64be 
     return b

lw642BS :: [ BN.Word64 ] -> BS.ByteString
lw642BS [] = C.pack ""
lw642BS lst = foldr (\t1 t2 -> (BS.concat $ L.toChunks $ BN.encode $ t1) `BS.append` t2) (BS.concat $ L.toChunks $  BN.encode $ head lst) lst
  
-- I've chosen to change the endian order here instead of in mkCache...could refactor later
calcDatasetItemBS :: V.Vector BS.ByteString -> Int -> State BS.ByteString BS.ByteString
calcDatasetItemBS cache i = do
   
   let n = V.length cache
         
   let mix0 = (cache V.! i)
   let mix1 = ( BS.concat $ L.toChunks  $ BN.encode $ G.runGet G.getWord64be (L.fromChunks [mix0])) `xorBS` ( BS.concat $ L.toChunks $ BN.encode i)
   let mix2 = bs2HashBS mix1

   put mix2
   
   forM_ [1..datasetParents-1] $ \j -> do
     mix <- get
     let mixW64 = IG.runGet bs2LW64 mix
     case mixW64 of
       IG.Finished bs mixW64res -> do
          let cacheIndex = fnv ((fromIntegral $ i :: BN.Word64) `xor` (fromIntegral $ j :: BN.Word64)) (mixW64res !! ((fromIntegral $ j :: Int) `mod` r) )
          let mixWith = IG.runGet bs2LW64 (cache V.! ( (fromIntegral $ cacheIndex :: Int) `mod` n))

          case mixWith of
            IG.Finished bs mixWithres -> do
               let newMixW64 = zipWith fnv mixW64res mixWithres
               put (lw642BS $ newMixW64)
            _  ->  error "element wrong length"
       _ -> error "element wrong length"


   finMix <- get
   return $ bs2HashBS $ finMix
   
   
   where r = fromIntegral $ hashBytes `div` wordBytes :: Int
