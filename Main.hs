
module Main where

import Crypto.Hash
import System.Endian
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as BN
import qualified Data.Binary.Strict.IncrementalGet as IG
import Cache
import Dataset
import qualified Data.Vector as V

main :: IO ()
main = do
           cache <- mkCache (2^12) (C.pack $ "seed")
         --  bs <- lcalcDatasetItemBS cache (0 :: Int)
{-
           let mix0 = (cache V.! 7)
           putStrLn . show $ mix0
           putStrLn . show $ BS.length mix0
           putStrLn . show $ ((C.take 8 mix0) `xorBS` (BS.concat $ L.toChunks $ BN.encode (7 :: Int) )) `BS.append` (C.drop 8 mix0)
           putStrLn . show $ BS.length $ ((C.take 8 mix0) `xorBS` (BS.concat $ L.toChunks $ BN.encode (7 :: Int) )) `BS.append` (C.drop 8 mix0)
      --     putStrLn . show $ calcDatasetItemBS cache 0
           putStrLn . show $ cacheFunc cache 0 (cache V.! 0,2)
           putStrLn . show $ BS.length $ fst $ cacheFunc cache 0 (cache V.! 0,2)

           putStrLn . show $ IG.runGet bs2LW64 (cache V.! 0)
           putStrLn . show $ BS.length $ bs2HashBS $ mix0
-}         let mix0 = (cache V.! (9 `mod` 64))
           let (IG.Finished _ mixl) = IG.runGet bs2LW32 $ mix0
	   let mixE = lw322BS $ map fromBE32 mixl
--           let mixE = lw322BS $ mixl
           let mix1 = ((C.take 4 mixE) `xorBS` (BS.concat $ L.toChunks $ BN.encode (fromIntegral $ 9 :: BN.Word32))) `BS.append` (C.drop 4 mixE)

	   putStrLn $ "mix0: " ++ (show $ IG.runGet bs2LW32 $ mix0)
           putStrLn $ "mix1: " ++ (show $ IG.runGet bs2LW32 $ mix1)

           let (IG.Finished _ mixl1) = IG.runGet bs2LW32 $ mix1
	   let mix1E = lw322BS $ map fromBE32 mixl1

	   let mixInit = bs2HashBS mix1
           putStrLn $ "mixInit: " ++ (show $ mixInit)
	   let mixW32 = IG.runGet bs2LW32 $ mixInit
	   putStrLn $ "mixW32: " ++ (show $ mixW32)


	   let mix1Init = bs2HashBS mix1E
           putStrLn $ "mix1Init: " ++ (show $ mix1Init)
	   let mix1W32 = IG.runGet bs2LW32 $ mix1Init
	   putStrLn $ "mixW32: " ++ (show $ mix1W32)
	   putStrLn . show $ calcDatasetItemBS cache 9

{-
           let b = B.pack [1,2,3,4]
           putStrLn $ show (hash b :: Digest SHA1)
           putStrLn $ show (hash b :: Digest SHA224)
           putStrLn $ show (hash b :: Digest SHA512)
           putStrLn $ show (hash b :: Digest SHA3_512)
           putStrLn $ show (hash b :: Digest MD5)
 -}        

