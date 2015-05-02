
module Hashimoto where


import Constants
import qualified Crypto.Hash.SHA3 as SHA3
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word
import qualified Data.Vector as V

import Util

--import Debug.Trace

-- type Dataset =  Repa.Array BN.Word32 Repa.DIM1 Int



{- def hashimoto(header, nonce, full_size, dataset_lookup):
    n = full_size / HASH_BYTES
    mixhashes = MIX_BYTES / HASH_BYTES
    # combine header+nonce into a 64 byte seed
    s = sha3_512(header + nonce[::-1])
    # start the mix with replicated s
    mix = []
    for _ in range(MIX_BYTES / HASH_BYTES):
        mix.extend(s)
    # mix in random dataset nodes
    for i in range(ACCESSES):
        p = fnv(i ^ s[0], mix[i % w]) % (n // mixhashes) * mixhashes
        newdata = []
        for j in range(MIX_BYTES / HASH_BYTES):
            newdata.extend(dataset_lookup(p + j))
        mix = map(fnv, mix, newdata)
    # compress mix
    cmix = []
    for i in range(0, len(mix), 4):
        cmix.append(fnv(fnv(fnv(mix[i], mix[i+1]), mix[i+2]), mix[i+3]))
    return {
        "mix digest": serialize_hash(cmix),
        "result": serialize_hash(sha3_256(s+cmix))
    }
-}

wordPack::[Word32]->B.ByteString
wordPack = B.concat . fmap (BL.toStrict . runPut . putWord32le) 

getWord::B.ByteString->Word32->Word32
getWord x i = runGet getWord32le $ BL.fromStrict $ B.take 4 $ B.drop (fromIntegral $ 4*i) x

hashimoto::B.ByteString->B.ByteString->Int->(Word32->B.ByteString)->(B.ByteString, B.ByteString)
hashimoto header nonce fullSize' dataset =
  (cmix, SHA3.hash 256 (s `B.append` cmix))
    where
      mixhashes = mixBytes `div` hashBytes
      s = SHA3.hash 512 $ header `B.append` B.reverse nonce
      mix = B.concat $ replicate (fromInteger mixhashes) s
      newmix = fst $ iterate (f (dataset, fullSize', mixhashes, s)) (mix, 0) !! 64
      cmix = repair $ map f2 [0,4..fromIntegral $ B.length newmix `div` 4 - 1]
      f2 i = getWord newmix i `fnv` getWord newmix (i + 1) `fnv`  getWord newmix (i + 2) `fnv` getWord newmix (i + 3)

f::(Word32->B.ByteString, Int, Integer, B.ByteString)->(B.ByteString, Word32)->(B.ByteString, Word32)
f (dataset, fullSize', mixhashes, s) (mix, i) =
  (repair $ zipWith fnv (shatter mix) (shatter newdata), i+1)
  where
    p = (fnv (i `xor` (runGet getWord32le $ BL.fromStrict $ B.take 4 s))
         (getWord mix (i `mod` fromInteger w))) `mod` (fromIntegral n `div` fromInteger mixhashes) * fromInteger mixhashes
    newdata = B.concat $ map dataset [fromIntegral p..fromIntegral p + fromInteger mixBytes `quot` fromInteger hashBytes-1]
    n = fullSize' `div` fromInteger hashBytes
    w = mixBytes `div` wordBytes





--hashimotoFull :: Int -> Dataset -> SHA -> Int
--hashimotoFull fullSize dataset header nonce = undefined

-- mixHash ::  -> SHA
