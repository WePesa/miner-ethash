
module Util (
  xorBS
  ) where

import qualified Data.Binary.Strict.IncrementalGet as IG
import Data.Bits
import qualified Data.ByteString as B
import Data.Word

xorBS :: B.ByteString -> B.ByteString -> B.ByteString
xorBS x y = B.pack $ B.zipWith xor x y

