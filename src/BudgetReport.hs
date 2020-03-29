module BudgetReport (T (..), Row (..), toCsv) where

import qualified Csv
import qualified Data.Csv
import Data.Csv ((.=))
import qualified Money
import qualified Priority
import Protolude

newtype T = T [Row] deriving (Show, Eq)

data Row
  = Row
      { priority :: Priority.T,
        spendQ1 :: Money.T,
        spendQ2 :: Money.T,
        spendQ3 :: Money.T,
        spendQ4 :: Money.T
      }
  deriving (Show, Eq)

instance Data.Csv.ToNamedRecord Row where
  toNamedRecord Row {priority, spendQ1, spendQ2, spendQ3, spendQ4} =
    Data.Csv.namedRecord
      [ "Priority" .= priority,
        "Spend Q1" .= spendQ1,
        "Spend Q2" .= spendQ2,
        "Spend Q3" .= spendQ3,
        "Spend Q4" .= spendQ4
      ]

toCsv :: T -> Text
toCsv (T rows) = Csv.encode ["Priority", "Spend Q1", "Spend Q2", "Spend Q3", "Spend Q4"] rows
