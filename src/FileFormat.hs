module FileFormat (dropByteOrderMark) where

import qualified Data.ByteString.Lazy as DBL
import Protolude

dropByteOrderMark :: DBL.ByteString -> DBL.ByteString
dropByteOrderMark byteString
  | DBL.take 3 byteString == DBL.pack [0xEF, 0xBB, 0xBF] = DBL.drop 3 byteString
  | otherwise = byteString
