{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.ByteString.Internal
import qualified Data.ByteString.Base16 as B16
import Data.Word
import qualified Data.Vector as V
import Numeric

--import Blockchain.Data.Address
--import Blockchain.Data.BlockDB
--import Blockchain.SHA
--import Blockchain.Database.MerklePatricia

import Cache
import Dataset
import Hashimoto


{-
05d30ed0ab6ab488eaf469ca6e9c3572f1624207044165c19763fb79e68ac8d7: Block #149280 05d30ed0ab6ab488eaf469ca6e9c3572f1624207044165c19763fb79e68ac8d7
    transactionsRoot: 
    receiptsRoot: 56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421
    difficulty: 21392572
    gasLimit: 3141592
    gasUsed: 0
    timestamp: 2015-04-22 06:20:40 UTC
    extraData: 0
    nonce: 3a48a28f51185258
            (no transactions)
            (no uncles)
-}

{-
theBlock =
  Block {
    blockBlockData=
       BlockData {
         blockDataParentHash = SHA 0x8fadac37a432aa49eac28c9339537e1ea6c57daa84954f4c55b35ae5bad60c09,
         blockDataUnclesHash = SHA 0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347,
         blockDataCoinbase = Address 0xcb26a56f091fe7a442b0b5cbfbd8975779629e83,
         blockDataStateRoot = SHAPtr $ fst $ B16.decode "d7da2b91c129aa4c8767ef88198a5b5ffc0980a83a846cfe9383821c0c98a324",
         blockDataTransactionsRoot = SHAPtr $ fst $ B16.decode "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
         blockDataReceiptsRoot = SHAPtr $ fst $ B16.decode "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
         blockDataLogBloom = undefined,
         blockDataDifficulty = 21392572,
         blockDataNumber = 149280,
         blockDataGasLimit = 3141592,
         blockDataGasUsed = 0,
         blockDataTimestamp = undefined,
         blockDataExtraData = undefined,
         blockDataNonce = undefined,
         blockDataMixHash = undefined
         },
    blockReceiptTransactions=[],
    blockBlockUncles=[]
    }
-}


encodeWord8::Word8->String
encodeWord8 c | c < 0x20 || c > 0x7e = "\\x" ++ showHex c ""
encodeWord8 c = [w2c c]

encodeByteString::B.ByteString->String
encodeByteString = (encodeWord8 =<<) . B.unpack

main :: IO ()
main = do
  --cache <- mkCache (2^12) "seed"
  cache <- mkCache 512 "seed"
  --putStrLn $ unlines $ map show $ map B16.encode $ V.toList cache
  --putStrLn $ show $ B16.encode $ calcDatasetItem cache 100
  let dataset = calcDataset 512 cache
  --putStrLn $ show $ map B16.encode $ V.toList dataset
  let (mixDigest, result) = hashimoto (B.pack [1,2,3,4]) (B.pack [1,2,3,4]) 512 ((dataset V.!) . fromIntegral)
  putStrLn $ "mixDigest: " ++ encodeByteString mixDigest
  putStrLn $ "result: " ++ encodeByteString result
