module Preprocessor (preprocess) where

import qualified BudgetRecords
import qualified Department
import qualified LineNumber
import qualified PriorityRecord
import Protolude
import qualified SalaryRecord
import qualified TeammateRecord

preprocess :: BudgetRecords.T -> BudgetRecords.T
preprocess budgetRecords = onlyDevRecords $ withLineNumbers budgetRecords

withLineNumbers :: BudgetRecords.T -> BudgetRecords.T
withLineNumbers
  BudgetRecords.T
    { BudgetRecords.priorityRecords,
      BudgetRecords.salaryRecords,
      BudgetRecords.teammateRecords
    } =
    BudgetRecords.T numberedPriorities numberedSalaries numberedTeammates
    where
      numberedPriorities = recordsWithLineNumbers numberPriority priorityRecords
      numberedSalaries = recordsWithLineNumbers numberSalary salaryRecords
      numberedTeammates = recordsWithLineNumbers numberTeammate teammateRecords
      numberPriority lineNumber priority = priority {PriorityRecord.lineNumber = lineNumber}
      numberSalary lineNumber salary = salary {SalaryRecord.lineNumber = lineNumber}
      numberTeammate lineNumber teammate = teammate {TeammateRecord.lineNumber = lineNumber}

onlyDevRecords :: BudgetRecords.T -> BudgetRecords.T
onlyDevRecords
  BudgetRecords.T
    { BudgetRecords.priorityRecords,
      BudgetRecords.salaryRecords,
      BudgetRecords.teammateRecords
    } =
    BudgetRecords.T priorityRecords salaryRecords $ onlyDevTeammates teammateRecords

onlyDevTeammates :: [TeammateRecord.T] -> [TeammateRecord.T]
onlyDevTeammates = filter $ not . Department.excludedFromDev . TeammateRecord.department

recordsWithLineNumbers :: (LineNumber.T -> a -> a) -> [a] -> [a]
recordsWithLineNumbers numberAssigner = zipWith numberAssigner lineNumbers
  where
    lineNumbers = iterate LineNumber.next 1
