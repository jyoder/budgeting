module Priority (T) where

import qualified Data.Csv
import Protolude

newtype T = T Text
  deriving (Show, Eq, Ord)
  deriving newtype (IsString, Data.Csv.FromField, Data.Csv.ToField)
