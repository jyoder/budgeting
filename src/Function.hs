module Function (T, toText) where

import qualified Data.Csv
import Protolude

newtype T = T Text
  deriving (Show, Eq, Ord)
  deriving newtype (IsString, Data.Csv.FromField)

toText :: T -> Text
toText (T text) = text
