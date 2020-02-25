module BudgetRecords (T (..)) where

import PriorityRecord
import Protolude
import SalaryRecord
import TeammateRecord

data T
  = T
      { priorityRecords :: [PriorityRecord],
        salaryRecords :: [SalaryRecord],
        teammateRecords :: [TeammateRecord]
      }
  deriving (Show)
