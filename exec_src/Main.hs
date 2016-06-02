{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

import Control.Monad
import qualified Data.Array.IO as A
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Array.IO as MA
import Data.ByteString.Internal
import Blockchain.Format
import Blockchain.Util
import Data.Word
import Foreign.Storable
import Numeric
import System.IO.MMap

import TimeIt

import Cache
import Constants
import Dataset
import Hashimoto
import Util

encodeWord8::Word8->String
encodeWord8 c | c < 0x20 || c > 0x7e = "\\x" ++ showHex c ""
encodeWord8 c = [w2c c]

encodeByteString::B.ByteString->String
encodeByteString = (encodeWord8 =<<) . B.unpack

word32Unpack::B.ByteString->[Word32]
word32Unpack s | B.null s = []
word32Unpack s | B.length s >= 4 = decode (BL.fromStrict $ B.take 4 s) : word32Unpack (B.drop 4 s)
word32Unpack _ = error "word32Unpack called for ByteString of length not a multiple of 4"

    --putStrLn "Generating cache"
    --cache <- getCache 0
    --let getItem = calcDatasetItem cache . fromIntegral
    --(mixDigest, result) <- hashimoto blockHash nonce fullSize' getItem
    --let res = byteString2Integer result

dagCompare :: Slice -> Slice -> IO Bool
dagCompare a b = do
  v1 <- MA.readArray a 0
  v2 <- MA.readArray b 0
  putStrLn $ "V1: " ++ (show v1)
  putStrLn $ "V2: " ++ (show v2)
  return (v1 == v2)

main :: IO ()
main = do
  putStrLn $ "Generating cache"
  cache <- mkCache (fromIntegral $ cacheSize 0) $ B.replicate 32 0
--  let dataset = calcDataset (fullSize 0) cache

  putStrLn $ "Loading file from disk.."
  s <- mmapFileByteString "full-R23-0000000000000000" Nothing
  putStrLn $ "..loaded " ++ (show $ B.length s) ++ " bytes"
  let getItem' i = A.newListArray (0,15) $ word32Unpack $ B.take 64 $ B.drop (64 * fromIntegral i) s

  let fullSize' = fromIntegral $ fullSize 0
      getItem   = calcDatasetItem cache . fromIntegral
      block'    = "85913a3057ea8bec78cd916871ca73802e77724e014dda65add3405d02240eb7" :: B.ByteString
      nonce'    = "539bd4979fef1ec4" :: B.ByteString
      diff'     = 17171480576 :: Integer

  putStrLn $ "DAG size is " ++ (show fullSize')

  --forM_ [0..1::Integer] $ \i -> do
  --  a <- getItem' i
  --  b <- getItem i
  --  res <- dagCompare a b
  --  if (res)
  --  then (putStrLn $ "match for " ++ (show i))
  --  else (putStrLn $ "mismatch for " ++ (show i))

  --block = B.pack [1,2,3,4] :: B.ByteString --"969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f59" 
  --nonce = B.pack [1,2,3,4] :: B.ByteString -- 6024642674226569000

  --block' = integer2ByteString 60414203290591505497886081091015673343483699986974505362697862788662685863607
  --nonce' = integer2ByteString 6024642674226569000 :: B.ByteString

  timeIt $ do
    forM_ [0..0::Integer] $ \_ -> do 
      --s <- mmapFileByteString "qqqq" Nothing
      --let getItem' i = A.newListArray (0,15) $ word32Unpack $ B.take 64 $ B.drop (64 * fromIntegral i) s
      putStrLn $ "Hashimoto from disk"
      (mixDigest, result) <- hashimoto block' nonce' fullSize' getItem'
      putStrLn $ "mixDigest: " ++ format mixDigest
      putStrLn $ "result: " ++ format result
      putStrLn $ "valid: " ++ show (verify (byteString2Integer result) diff')

      putStrLn $ "Hashimoto from cache"
      (mixDigest, result) <- hashimoto block' nonce' fullSize' getItem
      putStrLn $ "mixDigest: " ++ format mixDigest
      putStrLn $ "result: " ++ format result
      putStrLn $ "valid: " ++ show (verify (byteString2Integer result) diff')
      return ()
