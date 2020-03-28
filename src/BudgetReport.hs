module BudgetReport (T (..), Row (..)) where

import qualified Money
import qualified Priority
import Protolude

newtype T = T [Row] deriving (Show, Eq)

data Row
  = Row
      { priority :: Priority.T,
        spendQ1 :: Money.T
      }
  deriving (Show, Eq)
