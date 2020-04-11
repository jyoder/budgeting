module Role (T, toText) where

import qualified Data.Csv
import Protolude

newtype T = T Text
  deriving (Show, Eq, Ord)
  deriving newtype (IsString, Data.Csv.FromField)

instance Data.Csv.ToField T where
  toField role = Data.Csv.toField $ toText role

toText :: T -> Text
toText (T text) = text
