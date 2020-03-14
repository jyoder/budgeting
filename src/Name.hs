module Name (T) where

import qualified Data.Csv
import Protolude

newtype T = T Text
  deriving (Show, Eq, Ord)
  deriving newtype (IsString)
  deriving newtype (Data.Csv.FromField)
