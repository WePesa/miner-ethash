
module Dataset
       ( calcDatasetItemBS,
         cacheFunc,
         lw322BS
       )
    where

import Control.Monad
import Control.Monad.State
import Control.Monad.Primitive
import qualified Crypto.Hash.SHA3 as SHA3
import Constants
import Data.List 
import qualified Data.Binary as BN
import qualified Data.Binary.Get as G
import Data.Binary.Put
import qualified Data.Binary.Strict.IncrementalGet as IG
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B
import Data.Byteable
import Data.Bits
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Repr.Vector as RV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Data.Word
import System.Endian
import Cache

import Util

import Debug.Trace
    
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

    
lw322BS :: [ BN.Word32 ] -> B.ByteString
lw322BS [] = BC.pack ""
lw322BS lst = foldr (\t1 t2 -> (BL.toStrict $ BN.encode $ t1) `B.append` t2) (BL.toStrict $  BN.encode $ head lst) lst
       
calcDatasetItemBS :: V.Vector B.ByteString -> Word32 -> B.ByteString
calcDatasetItemBS cache i =
  --trace (show $ B16.encode mixInit) $
  --trace (show $ B16.encode $ fst $ mixList !! 1) $
  SHA3.hash 512 $ fst $ mixList !! 255
--calcDatasetItemBS cache i = bs2HashBS $ fst $ mixList !! (fromInteger $ datasetParents :: Int)
   where mixList = iterate (cacheFunc cache i) ( mixInit, 0 )
         mixInit = SHA3.hash 512 $
                       BL.toStrict (runPut (putWord32le i) `BL.append` BL.replicate 60 0)  `xorBS`
                       (cache V.! (fromIntegral i `mod` n))
         n = V.length cache

cacheFunc :: V.Vector B.ByteString -> Word32 -> ( B.ByteString, Word32 ) -> (B.ByteString, Word32)
cacheFunc cache i (mix, j) =
--  trace (show mixW32) $ 
--  trace (show mixWith) $ 
--  trace (show $ zipWith fnv mixLst mixWithLst) $ 
  (lint2BS $ zipWith fnv mixLst mixWithLst, j+1)
  where mixLst = mixW32
        mixWithLst = mixWith
        (IG.Finished _ mixW32) = IG.runGet (replicateM 16 IG.getWord32le) mix
        (IG.Finished _ mixWith) = IG.runGet (replicateM 16 IG.getWord32le) (cache V.! fromIntegral ( cacheIndex  `mod` n))
        cacheIndex = fnv (fromIntegral i `xor` j) (mixLst !! fromIntegral (j `mod` r))
        r = fromIntegral $ hashBytes `div` wordBytes :: Word32
        n = fromIntegral $ V.length cache::Word32

lint2BS :: [Word32] -> B.ByteString
lint2BS lst = B.concat $ fmap (BL.toStrict . runPut . putWord32le) lst


{-
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
-}

