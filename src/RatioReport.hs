module RatioReport (T (..), Row (..), toCsv) where

import qualified Csv
import qualified Data.Csv
import Data.Csv ((.=))
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
        "Devs Per Role Q1" .= devsPerRoleQ1 row,
        "Devs Per Role Q2" .= devsPerRoleQ2 row,
        "Devs Per Role Q3" .= devsPerRoleQ3 row,
        "Devs Per Role Q4" .= devsPerRoleQ4 row
      ]

toCsv :: T -> Text
toCsv (T rows) = Csv.encode ["Role", "Devs Per Role Q1", "Devs Per Role Q2", "Devs Per Role Q3", "Devs Per Role Q4"] rows
