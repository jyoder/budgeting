module Salary (T, make) where

import qualified Data.Csv
import Protolude

newtype T = T Double
  deriving (Show, Eq, Ord)
  deriving newtype (Num, Data.Csv.FromField)

make :: Double -> T
make = T
