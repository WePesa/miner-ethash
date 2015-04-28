
module Cache (
  Cache,
  mkCache
  ) where

import Control.Monad
import qualified Crypto.Hash.SHA3 as SHA3
import Constants
import qualified Data.Binary.Get as G
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as L
import Data.Bits
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V

import Util

type Cache = V.Vector BS.ByteString --We only need the full data set to be a Repa array.

mkCache :: Integer -> BS.ByteString -> IO Cache
mkCache cSize seed = do
  let n = cSize `div` hashBytes
      v = initDataSet n $ SHA3.hash 512 seed
  mv <- V.unsafeThaw v
  mix mv
  V.unsafeFreeze mv

{-
for _ in range(CACHE_ROUNDS):
        for i in range(n):
            v = o[i][0] % n
            o[i] = sha3_512(map(xor, o[(i-1+n) % n], o[v]))

-}

mix::MV.IOVector BS.ByteString -> IO ()
mix mx = do
  let n = MV.length mx
    
  replicateM_ cacheRounds $
    forM_ [0..(n-1)] $ \i -> do
      idex <-  MV.read mx i

      let v = fromIntegral (G.runGet G.getWord32le $ L.fromStrict idex) `mod` n

      m1 <- MV.read mx v
      m2 <- MV.read mx $ (i-1+n) `mod` n
      MV.write mx i $ SHA3.hash 512 $ xorBS m1 m2

initDataSet :: Integer -> BS.ByteString -> V.Vector BS.ByteString
initDataSet n | n > toInteger (maxBound::Int) =
  error "initDataSet called for value too large, you can no longer use Data.Vector"
initDataSet n = V.fromListN (fromInteger n) . iterate (SHA3.hash 512)
              
