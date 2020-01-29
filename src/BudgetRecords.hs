module BudgetRecords (BudgetRecords (..)) where

import InitiativeRecord
import Protolude
import SalaryRecord
import TeammateRecord

data BudgetRecords
  = BudgetRecords
      { initiativeRecords :: [InitiativeRecord],
        salaryRecords :: [SalaryRecord],
        teammateRecords :: [TeammateRecord]
      }
  deriving (Show, Eq)
