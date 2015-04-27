
module Constants where

{-

WORD_BYTES = 4                    # bytes in word
DATASET_BYTES_INIT = 2**30        # bytes in dataset at genesis
DATASET_BYTES_GROWTH = 2**23      # dataset growth per epoch
CACHE_BYTES_INIT = 2**24          # bytes in dataset at genesis
CACHE_BYTES_GROWTH = 2**17        # cache growth per epoch
CACHE_MULTIPLIER=1024             # Size of the DAG relative to the cache
EPOCH_LENGTH = 30000              # blocks per epoch
MIX_BYTES = 128                   # width of mix
HASH_BYTES = 64                   # hash length in bytes
DATASET_PARENTS = 256             # number of parents of each dataset element
CACHE_ROUNDS = 3                  # number of rounds in cache production
ACCESSES = 64                     # number of accesses in hashimoto loop

-}

import Math.NumberTheory.Primes.Testing
import Data.List

numBits = 512
wordBytes = 4
datasetBytesInit = 2^30
datasetBytesGrowth = 2^23
cacheBytesInit = 2^24
cacheBytesGrowth = 2^17
cacheMultiplier = 1024
epochLength = 30000
mixBytes = 128
hashBytes = 64
datasetParents = 256
cacheRounds = 3::Int
accesses = 64


{-
def get_cache_size(block_number):
    sz = CACHE_BYTES_INIT + CACHE_BYTES_GROWTH * (block_number // EPOCH_LENGTH)
    sz -= HASH_BYTES
    while not isprime(sz / HASH_BYTES):
        sz -= 2 * HASH_BYTES
    return sz
-}

cacheSize :: Integer -> Maybe Integer
cacheSize blockNumber = find (\t -> isPrime (t `div` hashBytes)) [(size - hashBytes),(size-3*hashBytes)..0]
  where
    size = cacheBytesInit + cacheBytesGrowth * (blockNumber `div` epochLength)
{-
def get_full_size(block_number):
    sz = DATASET_BYTES_INIT + DATASET_BYTES_GROWTH * (block_number // EPOCH_LENGTH)
    sz -= MIX_BYTES
    while not isprime(sz / MIX_BYTES):
        sz -= 2 * MIX_BYTES
    return sz
-}

fullSize :: Integer -> Maybe Integer
fullSize blockNumber = find (\t -> isPrime (t `div` mixBytes)) [(size - mixBytes),(size-3*mixBytes)..0]
  where
    size = datasetBytesInit + datasetBytesGrowth * (blockNumber `div` epochLength)
