
module Cache 
       ( mkCache,
         bs2LW32,
         xorBS,
         Cache
       )
    where

import Control.Monad
import qualified Crypto.Hash.SHA3 as SHA3
import Constants
import qualified Data.Binary as BN
import qualified Data.Binary.Get as G
import qualified Data.Binary.Strict.IncrementalGet as IG
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as L
import Data.Bits
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V

{-
def mkcache(cache_size, seed):
    n = cache_size // HASH_BYTES

    # Sequentially produce the initial dataset
    o = [sha3_512(seed)]
    for i in range(1, n):
        o.append(sha3_512(o[-1]))

    # Use a low-round version of randmemohash
    for _ in range(CACHE_ROUNDS):
        for i in range(n):
            v = o[i][0] % n
            o[i] = sha3_512(map(xor, o[(i-1+n) % n], o[v]))

    return o
-}


type Cache = V.Vector BS.ByteString

{-
V.Vector BS.ByteString is ok for now. We only need the full data set to be a Repa array.
-}


mkCache :: Integer -> BS.ByteString -> IO Cache
mkCache cSize seed = mix $ initDataSet n $ SHA3.hash 512 seed
  where
    n = cSize `div` hashBytes
    
bs2LW32 :: IG.Get [ BN.Word32 ] [ BN.Word32 ]
bs2LW32  = do
  replicateM 16 $ do
     b <- IG.getWord32be 
     return b

      
  
xorBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorBS b1 b2 = (BS.pack . BS.zipWith xor b1 ) $ b2

{-
for _ in range(CACHE_ROUNDS):
        for i in range(n):
            v = o[i][0] % n
            o[i] = sha3_512(map(xor, o[(i-1+n) % n], o[v]))

-}

mix ::V.Vector BS.ByteString -> IO (V.Vector BS.ByteString)
mix init' = do
    mx <- V.thaw init'
    let n = MV.length mx
    
    replicateM_ cacheRounds $
      forM_ [0..(n-1)] $ \i -> do
        idex <-  MV.read mx i

        let v = fromIntegral (G.runGet G.getWord32le $ L.fromStrict idex) `mod` n

        m1 <- MV.read mx v
        m2 <- MV.read mx $ (i-1+n) `mod` n
        MV.write mx i $ SHA3.hash 512 $ xorBS m1 m2
          
    fmx <- V.freeze mx
    return fmx
    

initDataSet :: Integer -> BS.ByteString -> V.Vector BS.ByteString
initDataSet n | n > toInteger (maxBound::Int) = error "initDataSet called for value too large, you can no longer use Data.Vector"
initDataSet n = V.fromListN (fromInteger n) . iterate (SHA3.hash 512)
              
