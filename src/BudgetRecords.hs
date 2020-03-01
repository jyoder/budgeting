module BudgetRecords (T (..)) where

import qualified PriorityRecord
import Protolude
import qualified SalaryRecord
import qualified TeammateRecord

data T
  = T
      { priorityRecords :: [PriorityRecord.T],
        salaryRecords :: [SalaryRecord.T],
        teammateRecords :: [TeammateRecord.T]
      }
  deriving (Show)
