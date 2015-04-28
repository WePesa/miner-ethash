
module Util (
  xorBS
  ) where

import Data.Bits
import qualified Data.ByteString as B

xorBS :: B.ByteString -> B.ByteString -> B.ByteString
xorBS x y = B.pack $ B.zipWith xor x y

