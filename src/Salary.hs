module Salary (T) where

import qualified Data.Csv
import Protolude

newtype T = T Double
  deriving (Show, Eq, Ord)
  deriving newtype (Num, Fractional, Data.Csv.FromField)
