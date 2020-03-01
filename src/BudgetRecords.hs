module BudgetRecords (T (..)) where

import qualified PriorityRecord
import Protolude
import SalaryRecord
import TeammateRecord

data T
  = T
      { priorityRecords :: [PriorityRecord.T],
        salaryRecords :: [SalaryRecord],
        teammateRecords :: [TeammateRecord]
      }
  deriving (Show)
