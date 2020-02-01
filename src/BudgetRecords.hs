module BudgetRecords (BudgetRecords (..)) where

import PriorityRecord
import Protolude
import SalaryRecord
import TeammateRecord

data BudgetRecords
  = BudgetRecords
      { priorityRecords :: [PriorityRecord],
        salaryRecords :: [SalaryRecord],
        teammateRecords :: [TeammateRecord]
      }
  deriving (Show, Eq)
