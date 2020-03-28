module BudgetReport (T (..), Row (..)) where

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
