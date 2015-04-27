
module Cache 
       ( mkCache,
         bs2HashBS,
         bs2LW32,
         xorBS,
         Cache(..)
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
import qualified Data.Binary.Strict.IncrementalGet as IG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
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


type Cache = IO (V.Vector BS.ByteString)

{-
IO (V.Vector BS.ByteString) is ok for now. We only need the full data set to be a Repa array.
-}


mkCache :: Int -> BS.ByteString -> Cache
mkCache cSize seed = do
  let init = initDataSet n (bs2HashBS seed)
  mx <- mix init

--  putStrLn . show $ V.map (IG.runGet bs2LW32) mx -- if you put this back in you can check against pyethash by calling fromBE32 on the entries

  putStrLn $ unlines $ map show $ map B16.encode $ V.toList init

  return mx
  where
    n = cSize `div` (fromIntegral $ hashBytes :: Int)
-- assume bs is 512 bits
-- done as stupidly as possible
-- probably a utility for later
    
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
mix init = do
    mx <- V.thaw init
    let n = MV.length mx
    
    replicateM_ cacheRounds $
      forM_ [0..(n-1)] $ \i -> do
        idex <-  MV.read mx i

        let v = fromIntegral (G.runGet G.getWord32le $ L.fromStrict idex) `mod` n

        m1 <- MV.read mx v :: IO BS.ByteString
        m2 <- MV.read mx ((i-1+n) `mod` n) :: IO BS.ByteString
        MV.write mx i (SHA3.hash 512 (xorBS m1 m2))
          
    fmx <- V.freeze mx
    return fmx
    

initDataSet :: Int -> BS.ByteString -> V.Vector BS.ByteString
initDataSet n = V.fromListN n . iterate (SHA3.hash 512)
                     
bs2HashBS :: BS.ByteString -> BS.ByteString
bs2HashBS = SHA3.hash 512




