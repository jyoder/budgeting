module BudgetRecords (T (..)) where

import qualified PriorityRecord
import Protolude
import qualified SalaryRecord
import TeammateRecord

data T
  = T
      { priorityRecords :: [PriorityRecord.T],
        salaryRecords :: [SalaryRecord.T],
        teammateRecords :: [TeammateRecord]
      }
  deriving (Show)
