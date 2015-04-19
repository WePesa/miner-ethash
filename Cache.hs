
module Cache 
       ( mkCache,
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


-- type Cache = Repa.Array BN.Word32 Repa.DIM1 Int

{-
IO (V.Vector BS.ByteString) is ok for now. We only need the full data set to be a Repa array.
-}


mkCache :: Int -> Integer -> IO (V.Vector BS.ByteString)
mkCache cSize seed = do
  let init = initDataSet n (integer2BS seed)
  mx <- mix init

  let bs = mx V.! 0
        
  putStrLn $ show . (IG.runGet bs2LW32) $ bs
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
    putStrLn $ "init length: " ++ show (V.length init)
    mx <- V.thaw init
    let n = MV.length(mx)
    putStrLn $ "length of thawed vector: " ++ show (n)
    
--    forM_ [0..(cacheRounds-1)] $ \notused -> do
    forM_ [0..(n-1)] $ \i -> do
        idex <-  MV.read mx i

        putStrLn $ "read from ith index: " ++ show idex
        
        let preIndex = (fromIntegral $ G.runGet G.getWord32le (L.fromChunks [idex])) :: Int
        let v = preIndex `mod` n

    --    putStrLn $ "round: " ++ show notused
        putStrLn $ "i: " ++ show i
        putStrLn $ "preIndex: " ++ show preIndex
        putStrLn $ "v: " ++ show v
        
        m1 <- MV.read mx v :: IO BS.ByteString
        m2 <- MV.read mx ((i-1+n) `mod` n) :: IO BS.ByteString
        MV.write mx i (bs2HashBS (xorBS m1 m2))
          
    fmx <- V.freeze mx
    return fmx
    

initDataSet :: Int -> BS.ByteString -> V.Vector BS.ByteString
initDataSet n seed = V.unfoldrN n (\t -> Just (t, bs2HashBS $ t) ) seed
                     
bs2HashBS :: BS.ByteString -> BS.ByteString
bs2HashBS bs = toBytes $ ( hash $ bs :: Digest SHA3_512)

integer2BS :: Integer -> BS.ByteString
integer2BS m = L.toStrict $ (BN.encode $ m) 
