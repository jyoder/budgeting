module BudgetReport (T (..), Spend (..)) where

import qualified Priority
import Protolude

newtype T = T [Spend] deriving (Show, Eq)

data Spend
  = Spend
      { priority :: Priority.T,
        spendQ1 :: Double
      }
  deriving (Show, Eq)
