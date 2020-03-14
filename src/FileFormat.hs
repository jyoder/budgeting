module FileFormat (dropByteOrderMark) where

import qualified Data.ByteString.Lazy as ByteString
import Protolude

dropByteOrderMark :: ByteString.ByteString -> ByteString.ByteString
dropByteOrderMark byteString
  | ByteString.take 3 byteString == ByteString.pack [0xEF, 0xBB, 0xBF] =
    ByteString.drop 3 byteString
  | otherwise = byteString
