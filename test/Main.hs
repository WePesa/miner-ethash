--{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Data.Functor
--import Data.List
--import Data.Monoid
--import System.Exit

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
--import Blockchain.Format
import Blockchain.Util
import Blockchain.ExtWord

import qualified Crypto.Hash.SHA3 as SHA3
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Blockchain.Data.RLP
import Blockchain.Data.BlockDB
--import Blockchain.Data.DataDefs

import Cache
import Constants
import Dataset
import Hashimoto
import Util

--import Numeric

blockSize1 :: Int
blockSize1 = fromIntegral $ fullSize 0

blockSize30001 :: Int
blockSize30001 = fromIntegral $ fullSize 30001

getItem :: Integral a => Cache -> a -> IO Slice
getItem cache = calcDatasetItem cache . fromIntegral 

-- from parity https://github.com/ethcore/parity/blob/master/ethash/src/compute.rs
--fn test_light_compute() {
--    let hash = [0xf5, 0x7e, 0x6f, 0x3a, 0xcf, 0xc0, 0xdd, 0x4b, 0x5b, 0xf2, 0xbe, 0xe4, 0x0a, 0xb3, 0x35, 0x8a, 0xa6, 0x87, 0x73, 0xa8, 0xd0, 0x9f, 0x5e, 0x59, 0x5e, 0xab, 0x55, 0x94, 0x05, 0x52, 0x7d, 0x72];
--    let mix_hash = [0x1f, 0xff, 0x04, 0xce, 0xc9, 0x41, 0x73, 0xfd, 0x59, 0x1e, 0x3d, 0x89, 0x60, 0xce, 0x6b, 0xdf, 0x8b, 0x19, 0x71, 0x04, 0x8c, 0x71, 0xff, 0x93, 0x7b, 0xb2, 0xd3, 0x2a, 0x64, 0x31, 0xab, 0x6d];
--    let boundary = [0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x3e, 0x9b, 0x6c, 0x69, 0xbc, 0x2c, 0xe2, 0xa2, 0x4a, 0x8e, 0x95, 0x69, 0xef, 0xc7, 0xd7, 0x1b, 0x33, 0x35, 0xdf, 0x36, 0x8c, 0x9a, 0xe9, 0x7e, 0x53, 0x84];
--    let nonce = 0xd7b3ac70a301a249;
--    // difficulty = 0x085657254bd9u64;
--    let light = Light::new(486382);
--    let result = light_compute(&light, &hash, nonce);
--    assert_eq!(result.mix_hash[..], mix_hash[..]);
--    assert_eq!(result.value[..], boundary[..]);
--}

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

fakeBlock666 :: TestBlock
fakeBlock666 = TestBlock
                    (BS8.pack "0000000000000000000000000000000000000000000000000000000000000001")
                    (BS8.pack "0000000000000000000000000000000000000000000000000000000000000001")
                    (BS8.pack "0000000000000001")
                    1
                    666

parityBlock :: TestBlock
parityBlock = TestBlock
                    (BS8.pack "f57e6f3acfc0dd4b5bf2bee40ab3358aa68773a8d09f5e595eab559405527d72")
                    (BS8.pack "1fff04cec94173fd591e3d8960ce6bdf8b1971048c71ff937bb2d32a6431ab6d")
                    (BS8.pack "d7b3ac70a301a249")
                    9166922271705 -- 085657254bd9u64
                    1

frontierBlock1 :: TestBlock
frontierBlock1 = TestBlock
                    (BS8.pack "85913a3057ea8bec78cd916871ca73802e77724e014dda65add3405d02240eb7")
                    (BS8.pack "969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f59")
                    (BS8.pack "539bd4979fef1ec4")
                    -- (integer2ByteString 6024642674226569000) -- 0x539bd4979fef1ec4
                    17171480576 
                    1

testBlock22 :: TestBlock
testBlock22 = TestBlock
                    (BS8.pack "372eca2454ead349c3df0ab5d00b0b706b23e49d469387db91811cee0358fc6d")
                    (BS8.pack "2f74cdeb198af0b9abe65d22d372e22fb2d474371774a9583c1cc427a07939f5")
                    (BS8.pack "495732e0ed7a801c")
                    --(integer2ByteString 5284748629380857884) -- 0x495732e0ed7a801c
                    132416
                    22

testBlock30001 :: TestBlock
testBlock30001 = TestBlock
                    (BS8.pack "7e44356ee3441623bc72a683fd3708fdf75e971bbe294f33e539eedad4b92b34")
                    (BS8.pack "144b180aad09ae3c81fb07be92c8e6351b5646dda80e6844ae1b697e55ddde84")
                    (BS8.pack "318df1c8adef7e5e")
                    --(integer2ByteString 3570775923788578398) -- 0x318df1c8adef7e5e
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
    let n1 = B16.encode $ B.pack $ word64ToBytes $ 6024642674226569000 :: B.ByteString
    --let n1 = B16.encode $ word64ToBytes $ 6024642674226569000 :: B.ByteString
    -- let n1 = BS8.pack $ "539bd4979fef1f28" -- 539bd4979fef1ec4
    let n2 = (nonce frontierBlock1)
    assertEqual "testing if nonce is encoded correctly" n1 n2

testIntegerEncoding2 :: Assertion
testIntegerEncoding2 = do
    let n1 = B16.encode $ integer2ByteString $ 5284748629380857884 
    let n2 = BS8.pack $ "495732e0ed7a801c"
    assertEqual "testing if nonce is right (2)" n1 n2

testSHA_0 :: Assertion
testSHA_0  = do
    let sha0 = BS8.pack $ "73993b68fc61f222b6f0fbae04828dd470a76983488a668fcfeccab5ab21ed7c3e517e1132bb35263ce5c99fdbc9778d5638935f60a942dd086f4c8182a53322"
    let zero = BS8.pack $ "00000000000000000000000000000000" -- B.replicate 32 0
    let hash = B16.encode $ SHA3.hash 512 $ zero
    assertEqual "Testing SHA512(0)" sha0 hash

testSHA512 :: Assertion
testSHA512 = do
    let hash = SHA3.hash 512 $ hashNoNonce `B.append` B.reverse nonce'
    assertEqual "Testing if sha(nonce, hash) is correct" (sha) (B16.encode hash)
        where 
            sha     = BS8.pack $ "de19459ffc8cf302840fa7310a75ad3e4b859dc904710671a4829bb83bd03c5e172428d66a6334fa65c573a3ca684bc5ea37b122f0ab34fd0e12c363059e8db1"
            hashNoNonce = BS8.pack $ "372eca2454ead349c3df0ab5d00b0b706b23e49d469387db91811cee0358fc6d"
            nonce'   = B16.encode $ integer2ByteString $ 5284748629380857884  
            --nonce'  = BS8.pack $ "495732e0ed7a801c"

shaVerify :: String -> B.ByteString
shaVerify x = B16.encode $ SHA3.hash 512 $ fst $ B16.decode $ BS8.pack x

testSHA :: TestBlock -> B.ByteString -> Assertion
testSHA b sha = do
    --  s' = header `B.append` (B16.encode $ B.reverse $ fst $ B16.decode nonce)
    --  s = SHA3.hash 512 $ fst $ B16.decode $ s'
    let hash = SHA3.hash 512 $ fst $ B16.decode $ (minerHash b) `B.append` (B16.encode $ B.reverse $ fst $ B16.decode (nonce b))
    assertEqual ("Testing if sha(nonce, hash) is correct for block " ++ (show $ number b)) (sha) (B16.encode hash)


-- According to `geth` the mixDigest for block #1 is 969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f59
-- Here we're testing if hashimoto returns this for the hash and nonce also taken from `geth`
testMixDigest :: Cache -> TestBlock -> Int -> Assertion
testMixDigest cache b blockSize = do
    (digest, result) <- hashimoto (minerHash b) (nonce b) blockSize (getItem cache)
    --let res = (byteString2Integer $ result)
    --let diff = (difficulty b)
    --let idiff = invDiff diff
    --let outStr =  "\n\tPOW: " ++ show res ++ " < " ++ show idiff ++ " = " ++ show (res < idiff)
    --           ++ "\n\tord res: " ++ show (length $ show res) ++ " ord idiff: " ++ show (length $ show idiff)
    --assertBool outStr (res * diff < (2::Integer)^(256::Integer))
    assertEqual ("testing if mixDigest is correct for block #" ++ (show (number b))) (mixDigest b) (B16.encode digest)
    assertEqual "testing if nonce satisfies difficulty check for block #1: w. verify':" True (verify' (byteString2Integer result) (difficulty b))
    assertEqual "testing if nonce satisfies difficulty check for block #1: w. verify" True (verify (byteString2Integer result) (difficulty b))

testMixDigest' :: Cache -> TestBlock -> Assertion
testMixDigest' cache b = do
    (digest, result) <- hashimoto' (minerHash b) blockSize1 (getItem cache)
    assertEqual ("testing if mixDigest' is correct for block #" ++ (show (number b))) (mixDigest b) (B16.encode digest)
    assertEqual "testing if nonce satisfies difficulty check for block #1: w. verify':" True (verify' (byteString2Integer result) (difficulty b))
    assertEqual "testing if nonce satisfies difficulty check for block #1: w. verify" True (verify (byteString2Integer result) (difficulty b))


main :: IO ()
main = do
    cache <- mkCache (fromIntegral $ cacheSize 0) $ B.replicate 32 0
    cache1 <-  mkCache (fromIntegral $ cacheSize 3001) $ B.replicate 32 0
    defaultMainWithOpts [ 
                          testCase "test seedHash1" testSeedHash1
                        , testCase "test minerHash" testMinerHash
                        --, testCase "test nonce"     testIntegerEncoding
                        , testCase "test nonce (2)" testIntegerEncoding2
                        , testCase "test sha(0)"    testSHA_0
                        --, testCase "test sha"       testSHA512
                        --, testCase "test sha fakeBlock666" (testSHA fakeBlock666 (BS8.pack "d618890d9eeebc3156a1176fc7f210d2097baf3dd2b8c915f342abf422ccc79fa340ae4fae48f261e62dcb6f66b92a1c1681f6e67527201f18085e0189b7e8b1" ) )
                        , testCase "test sha testBlock22" (testSHA testBlock22 (BS8.pack "de19459ffc8cf302840fa7310a75ad3e4b859dc904710671a4829bb83bd03c5e172428d66a6334fa65c573a3ca684bc5ea37b122f0ab34fd0e12c363059e8db1" ) )
                        --, testCase "test fakeBlock666" (testMixDigest cache fakeBlock666)
                        , testCase "test testBlock22" (testMixDigest cache testBlock22 blockSize1)
                        , testCase "test testBlock30001" (testMixDigest cache1 testBlock30001 blockSize30001)
                        --, testCase "test parityBlock" (testMixDigest cache parityBlock)
                        , testCase "test frontierBlock1" (testMixDigest cache frontierBlock1 blockSize1)
                        --, testCase "test' fakeBlock666" (testMixDigest' cache fakeBlock666)
                        --, testCase "test' testBlock22" (testMixDigest' cache testBlock22)
                        --, testCase "test' frontierBlock1" (testMixDigest' cache frontierBlock1)
                        ] mempty
