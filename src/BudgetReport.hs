module BudgetReport (T (..), Spend (..)) where

import qualified Money
import qualified Priority
import Protolude

newtype T = T [Spend] deriving (Show, Eq)

data Spend
  = Spend
      { priority :: Priority.T,
        spendQ1 :: Money.T
      }
  deriving (Show, Eq)
