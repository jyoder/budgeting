module Money (T, toDouble) where

import qualified Data.Csv
import qualified Data.Double.Conversion.Text
import Protolude

newtype T = T Double
  deriving (Show, Eq, Ord)
  deriving newtype (Num, Fractional, Data.Csv.FromField)

instance Data.Csv.ToField T where
  toField money = Data.Csv.toField $ toText money

toDouble :: T -> Double
toDouble (T value) = value

toText :: T -> Text
toText (T value) = Data.Double.Conversion.Text.toFixed 2 (millions value)

millions :: Double -> Double
millions value = value / 1000000.00
