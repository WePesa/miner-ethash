
module Hashimoto where


import Constants
import qualified Crypto.Hash.SHA3 as SHA3
import qualified Data.Array.Repa as Repa
import qualified Data.Binary as BN
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.Vector as V

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

hashimoto::B.ByteString->B.ByteString->Int->V.Vector B.ByteString->(B.ByteString, B.ByteString)
hashimoto _ _ _ _ = (cmix, SHA3.hash 256 (s `B.append` cmix))
    where
      cmix = undefined
      --cmix = map 
      --f = mix ! i `fnv` mix ! (i + 1) `fnv`  mix ! (i + 2) `fnv`  mix ! (i + 3)
      s = undefined




--hashimotoFull :: Int -> Dataset -> SHA -> Int
--hashimotoFull fullSize dataset header nonce = undefined

-- mixHash ::  -> SHA
