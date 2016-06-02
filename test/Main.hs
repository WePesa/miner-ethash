--{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor
import Data.List
import Data.Monoid
import System.Exit

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
import Blockchain.Format
import Blockchain.Util
import Blockchain.ExtWord

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Blockchain.Data.RLP
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs

import Cache
import Constants
import Dataset
import Hashimoto
import Util

import Numeric

blockSize1 = fromIntegral $ fullSize 0
blockSize30001 = fromIntegral $ fullSize 30001
getItem cache = calcDatasetItem cache . fromIntegral

-- output from geth on frontier follows:
{-

> debug.printBlock(1)

"Block(#1): Size: 537.00 B {
MinerHash: 85913a3057ea8bec78cd916871ca73802e77724e014dda65add3405d02240eb7
Header(88e96d4537bea4d9c05d12549907b32561d3bf31f45aae734cdc119f13406cb6):
[
    ParentHash: d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3
    UncleHash:  1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347
    Coinbase:   05a56e2d52c817161883f50c441c3228cfe54d9f
    Root:       d67e4d450343046425ae4271474353857ab860dbc0a1dde64b41b5cd3a532bf3
    TxSha       56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421
    ReceiptSha: 56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421
    Bloom:      00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    Difficulty: 17171480576
    Number:     1
    GasLimit:   5000
    GasUsed:    0
    Time:       1438269988
    Extra:      Geth/v1.0.0/linux/go1.4.2
    MixDigest:  969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f59
    Nonce:      539bd4979fef1ec4
]
Transactions:
[]
Uncles:
[]
}"

> debug.getBlockRlp(1)

"f90216f90211a0d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d493479405a56e2d52c817161883f50c441c3228cfe54d9fa0d67e4d450343046425ae4271474353857ab860dbc0a1dde64b41b5cd3a532bf3a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421b90100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008503ff80000001821388808455ba422499476574682f76312e302e302f6c696e75782f676f312e342e32a0969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f5988539bd4979fef1ec4c0c0"

     // from proof of concept nine testnet, epoch 0
    {
        number:      22,
        hashNoNonce: common.HexToHash("372eca2454ead349c3df0ab5d00b0b706b23e49d469387db91811cee0358fc6d"),
        difficulty:  big.NewInt(132416),
        nonce:       0x495732e0ed7a801c,
        mixDigest:   common.HexToHash("2f74cdeb198af0b9abe65d22d372e22fb2d474371774a9583c1cc427a07939f5"),
    },

    // from proof of concept nine testnet, epoch 1
    {
        number:      30001,
        hashNoNonce: common.HexToHash("7e44356ee3441623bc72a683fd3708fdf75e971bbe294f33e539eedad4b92b34"),
        difficulty:  big.NewInt(1532671),
        nonce:       0x318df1c8adef7e5e,
        mixDigest:   common.HexToHash("144b180aad09ae3c81fb07be92c8e6351b5646dda80e6844ae1b697e55ddde84"),
    },


-}

data TestBlock = TestBlock {
                          minerHash  :: B.ByteString
                        , mixDigest  :: B.ByteString
                        , nonce      :: B.ByteString
                        , difficulty :: Integer
                        , number     :: Integer
                    }

frontierBlock1 = TestBlock
                    (BS8.pack "85913a3057ea8bec78cd916871ca73802e77724e014dda65add3405d02240eb7")
                    (BS8.pack "969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f59")
                    (integer2ByteString 6024642674226569000)
                    17171480576 --539bd4979fef1ec4
                    1

testBlock22 = TestBlock
                    (BS8.pack "372eca2454ead349c3df0ab5d00b0b706b23e49d469387db91811cee0358fc6d")
                    (BS8.pack "2f74cdeb198af0b9abe65d22d372e22fb2d474371774a9583c1cc427a07939f5")
                    (integer2ByteString 5284748629380857884)
                    132416
                    22

testBlock30001 = TestBlock
                    (BS8.pack "7e44356ee3441623bc72a683fd3708fdf75e971bbe294f33e539eedad4b92b34")
                    (BS8.pack "144b180aad09ae3c81fb07be92c8e6351b5646dda80e6844ae1b697e55ddde84")
                    (integer2ByteString 3570775923788578398)
                    1532671
                    30001

gethRLPBlock1 :: B.ByteString
gethRLPBlock1 = BS8.pack "f90216f90211a0d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d493479405a56e2d52c817161883f50c441c3228cfe54d9fa0d67e4d450343046425ae4271474353857ab860dbc0a1dde64b41b5cd3a532bf3a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421b90100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008503ff80000001821388808455ba422499476574682f76312e302e302f6c696e75782f676f312e342e32a0969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f5988539bd4979fef1ec4c0c0"
--gethMixDigest = BS8.pack "969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f59"
--minerHashBlock1 = BS8.pack "85913a3057ea8bec78cd916871ca73802e77724e014dda65add3405d02240eb7"
--nonceBlock1     = integer2ByteString 6024642674226569000 :: B.ByteString
--diffBlock1      = 17171480576 :: Integer --539bd4979fef1ec4

testSeedHash1 :: Assertion
testSeedHash1 = do
    assertEqual "testing if epoch 1 is correct" "0x0000000000000000000000000000000000000000000000000000000000000000" "0x0000000000000000000000000000000000000000000000000000000000000000"
    assertEqual "testing epoch 30000 is correct" "0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563" "0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563"

testMinerHash :: Assertion
testMinerHash = do
    let gb = rlpDecode $ rlpDeserialize $ fst $ B16.decode gethRLPBlock1 :: Block
    let bh1 =  B16.encode $ shaify' $ blockDataNonce2RLPHardCode (blockBlockData gb)
    let bh2 =  B16.encode $ shaify' $ blockDataNonce2RLP         (blockBlockData gb)
    assertEqual "blockNonce2RLP is independent of hardcoded strings and integer" bh1 bh2
    assertEqual "testing if blockNonce2RLP is correct for block #1:" (minerHash frontierBlock1) bh2

testIntegerEncoding :: Assertion
testIntegerEncoding = do
    -- n1 was found in an older, working version of ethash
    let n1 = B.pack $ word64ToBytes $ 6024642674226569000 :: B.ByteString
    let n2 = (nonce frontierBlock1)
    assertEqual "testing if nonce is encoded correctly" n1 n2

-- According to `geth` the mixDigest for block #1 is 969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f59
-- Here we're testing if hashimoto returns this for the hash and nonce also taken from `geth`
testMixDigest :: Cache -> TestBlock -> Assertion
testMixDigest cache b = do
    (digest, result) <- hashimoto (minerHash b) (nonce b) blockSize1 (getItem cache)
    let res = (byteString2Integer $ result)
    let diff = (difficulty b)
    let idiff = invDiff diff
    let outStr =  "\n\tPOW: " ++ show res ++ " < " ++ show idiff ++ " = " ++ show (res < idiff)
               ++ "\n\tord res: " ++ show (length $ show res) ++ " ord idiff: " ++ show (length $ show idiff)
    --assertBool outStr (res * diff < (2::Integer)^(256::Integer))
    assertEqual ("testing if mixDigest is correct for block #" ++ (show (number b))) (mixDigest b) (B16.encode digest)
    assertEqual "testing if nonce satisfies difficulty check for block #1: w. verify':" True (verify' (byteString2Integer result) (difficulty b))
    assertEqual "testing if nonce satisfies difficulty check for block #1: w. verify" True (verify (byteString2Integer result) (difficulty b))

main :: IO ()
main = do
    cache <- mkCache (fromIntegral $ cacheSize 0) $ B.replicate 32 0
    defaultMainWithOpts [ 
                          testCase "test seedHash1" testSeedHash1
                        , testCase "test minerHash" testMinerHash
                        , testCase "test nonce"     testIntegerEncoding
                        , testCase "test testBlock22" (testMixDigest cache testBlock22)
                        , testCase "test frontierBlock1" (testMixDigest cache frontierBlock1)
                        ] mempty
