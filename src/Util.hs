
module Util (
  xorBS,
  fnv,
  shatter,
  repair, 
  verify
  ) where

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word

xorBS :: B.ByteString -> B.ByteString -> B.ByteString
xorBS x y = B.pack $ B.zipWith xor x y


fnvPrime::Word32
fnvPrime = 16777619

fnv::Word32->Word32->Word32
fnv v1 v2 = v1 * fnvPrime `xor` v2

shatter::B.ByteString->[Word32]
shatter x = runGet (replicateM len getWord32le) . BL.fromStrict $ x
  where
    len = B.length x `div` 4
    

repair::[Word32]->B.ByteString
repair = B.concat . fmap (BL.toStrict . runPut . putWord32le) 

verify :: Integer -> Integer -> Bool
verify n d = n < diff
        where diff = invDiff d
              invDiff d = round diff' :: Integer
              diff' = (2^256) / (fromIntegral d) :: Double
