module Team (T, make) where

import Protolude

newtype T = T Text
  deriving (Show, Eq, Ord)
  deriving newtype (IsString)

make :: Text -> T
make = T
