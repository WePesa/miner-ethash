{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor
import Data.List
import Data.Monoid
import System.Exit

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as B
import Blockchain.Format
import Blockchain.Util

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Cache
import Constants
import Dataset
import Hashimoto
import Util

blockSize = fromIntegral $ fullSize 0
getItem cache = calcDatasetItem cache . fromIntegral

minerHashBlock1 = "85913a3057ea8bec78cd916871ca73802e77724e014dda65add3405d02240eb7" :: B.ByteString
nonceBlock1     = integer2ByteString 6024642674226569000 :: B.ByteString
diffBlock1      = 17171480576 :: Integer

testSeedHash1 :: Assertion
testSeedHash1 = do
    assertEqual "testing if epoch 1 is correct" "0x0000000000000000000000000000000000000000000000000000000000000000" 1

testSeedHash2 :: Assertion
testSeedHash2 = do
    assertEqual "testing epoch 30000 is correct" "0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563" 1

testMinerHash :: Assertion
testMinerHash = do
    let blockHash = "85913a3057ea8bec78cd916871ca73802e77724e014dda65add3405d02240eb7" :: B.ByteString -- use shaify etc here
    assertEqual "testing if minerHash is correct for block #1:" "85913a3057ea8bec78cd916871ca73802e77724e014dda65add3405d02240eb7" blockHash 


-- According to `geth` the mixDigest for block #1 is 969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f59
-- Here we're testing if hashimoto returns this for the hash and nonce also taken from `geth`
testMixDigest :: Cache -> Assertion
testMixDigest cache = do
    (mixDigest, result) <- hashimoto minerHashBlock1 nonceBlock1 blockSize (getItem cache)
    assertEqual "testing if mixHash is correct for block #1:" "969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f59" (format mixDigest)

testBlock1 :: Cache -> Assertion
testBlock1 cache = do
    (mixDigest, result) <- hashimoto minerHashBlock1 nonceBlock1 blockSize (getItem cache)
    assertEqual "testing if nonce satisfies difficulty check for block #1:" True (verify (byteString2Integer result) diffBlock1)

testNumber :: Assertion
testNumber = do
    let n = 20::Integer
    assertEqual "rlp encoding failed for small number" (n) n

main :: IO ()
main = do
    cache <- mkCache (fromIntegral $ cacheSize 0) $ B.replicate 32 0
    defaultMainWithOpts [ testCase "test seedHash1" testSeedHash1
                        , testCase "test seedHash2" testSeedHash2
                        , testCase "test minerHash" testMinerHash
                        , testCase "test mixHash" (testMixDigest cache)
                        , testCase "test nonce for block #1" (testBlock1 cache)] mempty
