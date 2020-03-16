module Team (T, make, toText) where

import qualified Data.Csv
import Protolude

newtype T = T Text
  deriving (Show, Eq, Ord)
  deriving newtype (IsString, Data.Csv.FromField)

make :: Text -> T
make = T

toText :: T -> Text
toText (T text) = text
