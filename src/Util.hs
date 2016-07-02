
module Util (
  xorBS,
  fnv,
  shatter,
  repair, 
  verify,
  verify',
  --invDiff,
  blockDataNonce2RLP,
  blockDataNonce2RLPHardCode,
  shaify,
  shaify'
  ) where

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString.Base16 as B16
import Data.Word
import Data.Time.Clock.POSIX

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA3 as SHA3

import Blockchain.Data.BlockDB
import Blockchain.Data.RLP
import Blockchain.Data.DataDefs()
import Blockchain.Util

import Numeric

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
verify = verify'
-- verify n d = n < diff
--        where diff = invDiff' d
--              invDiff' d = round diff' :: Integer
--              diff' = (2^256 :: Int) / (fromIntegral d) :: Double

verify' :: Integer -> Integer -> Bool
verify' val diff = val * diff < ((2::Integer)^(256::Integer) :: Integer)

--invDiff :: Integer -> Integer
--invDiff d = round diff' :: Integer
--   where diff' = (2^256::Integer) / (fromIntegral d) :: Double

shaify :: RLPObject -> B.ByteString
shaify x = SHA256.hash $ SHA256.hash $ rlpSerialize x

shaify' :: RLPObject -> B.ByteString
shaify' x = SHA3.hash 256 $ rlpSerialize x

{-

from `geth` spec:

func (h *Header) HashNoNonce() common.Hash {
      return rlpHash([]interface{}{
            h.ParentHash,
            h.UncleHash,
            h.Coinbase,
            h.Root,
            h.TxHash,
            h.ReceiptHash,
            h.Bloom,
            h.Difficulty,
            h.Number,
            h.GasLimit,
            h.GasUsed,
            h.Time,
            h.Extra,
      })
}

-}

blockDataNonce2RLPHardCode :: BlockData -> RLPObject
blockDataNonce2RLPHardCode bd =
  RLPArray [
      rlpEncode $ blockDataParentHash bd,
      rlpEncode $ blockDataUnclesHash bd,
      rlpEncode $ blockDataCoinbase bd,
      rlpEncode $ blockDataStateRoot bd,
      rlpEncode $ (integer2ByteString $ fst $ head $ readHex "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421" :: B.ByteString), --blockDataTransactionsRoot bd,
      rlpEncode $ (integer2ByteString $ fst $ head $ readHex "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421" :: B.ByteString), --blockDataReceiptsRoot bd,
      rlpEncode $ blockDataLogBloom bd,
      rlpEncode $ blockDataDifficulty bd,
      rlpEncode $ blockDataNumber bd,
      rlpEncode $ blockDataGasLimit bd,
      rlpEncode $ blockDataGasUsed bd,
      rlpEncode (round $ utcTimeToPOSIXSeconds $ blockDataTimestamp bd::Integer),
      rlpEncode $ ("Geth/v1.0.0/linux/go1.4.2" :: String) --blockDataExtraData bd
      ]

blockDataNonce2RLP :: BlockData -> RLPObject
blockDataNonce2RLP bd =
  RLPArray [
      rlpEncode $ blockDataParentHash bd,
      rlpEncode $ blockDataUnclesHash bd,
      rlpEncode $ blockDataCoinbase bd,
      rlpEncode $ blockDataStateRoot bd,
      rlpEncode $ blockDataTransactionsRoot bd,
      rlpEncode $ blockDataReceiptsRoot bd,
      rlpEncode $ blockDataLogBloom bd,
      rlpEncode $ blockDataDifficulty bd,
      rlpEncode $ blockDataNumber bd,
      rlpEncode $ blockDataGasLimit bd,
      rlpEncode $ blockDataGasUsed bd,
      rlpEncode (round $ utcTimeToPOSIXSeconds $ blockDataTimestamp bd::Integer),
      rlpEncode $ blockDataExtraData bd
      ]
