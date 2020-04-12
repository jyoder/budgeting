module RatioReport (T (..), Row (..), toCsv) where

import qualified Csv
import qualified Data.Csv
import Data.Csv ((.=))
import qualified Data.Double.Conversion.Text
import Protolude
import qualified Role

newtype T = T [Row] deriving (Show, Eq)

data Row
  = Row
      { role :: !Role.T,
        devsPerRoleQ1 :: !Double,
        devsPerRoleQ2 :: !Double,
        devsPerRoleQ3 :: !Double,
        devsPerRoleQ4 :: !Double
      }
  deriving (Show, Eq)

instance Data.Csv.ToNamedRecord Row where
  toNamedRecord row =
    Data.Csv.namedRecord
      [ "Role" .= role row,
        "SEs Per Role Q1" .= format (devsPerRoleQ1 row),
        "SEs Per Role Q2" .= format (devsPerRoleQ2 row),
        "SEs Per Role Q3" .= format (devsPerRoleQ3 row),
        "SEs Per Role Q4" .= format (devsPerRoleQ4 row)
      ]

toCsv :: T -> Text
toCsv (T rows) =
  Csv.encode
    [ "Role",
      "SEs Per Role Q1",
      "SEs Per Role Q2",
      "SEs Per Role Q3",
      "SEs Per Role Q4"
    ]
    rows

format :: Double -> Text
format = Data.Double.Conversion.Text.toFixed 1
