module MockFileSystem (T, make, read) where

import qualified Data.Map.Strict as Map
import qualified Path
import Protolude
import qualified Result

newtype T = T (Map Path.T (Result.T Text))

make :: [(Path.T, Result.T Text)] -> T
make = T . Map.fromList

read :: T -> Path.T -> Result.T Text
read (T fileMap) path = maybe notFound identity (Map.lookup path fileMap)
  where
    notFound = Result.error $ "File not found: " <> Path.toText path
