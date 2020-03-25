module ReportGenerator (generate) where

import qualified BudgetRecords
import qualified BudgetReport
import qualified Control.Applicative
import qualified PriorityRecord
import Protolude
import qualified Salary
import qualified SalaryRecord
import qualified TeammateRecord
import qualified Teams

type RecordTuple = (PriorityRecord.T, SalaryRecord.T, TeammateRecord.T)

generate :: BudgetRecords.T -> BudgetReport.T
generate budgetRecords = BudgetReport.T spendsQ1
  where
    spendsQ1 = map makeSpend recordTuplesQ1
    recordTuplesQ1 = joinBudgetRecordsQ1 budgetRecords

makeSpend :: [RecordTuple] -> BudgetReport.Spend
makeSpend [] = BudgetReport.Spend "Undefined" 0.00
makeSpend (r : rs) = BudgetReport.Spend priority spendQ1
  where
    spendQ1 = sum $ adjustedSalariesQ1 $ r : rs
    priority = PriorityRecord.priorityQ1 (fst3 r)
    adjustedSalariesQ1 = map adjustedSalaryQ1
    adjustedSalaryQ1 (_, salary, teammate) = salaryQ1 salary / teamCount teammate
    salaryQ1 = Salary.toDouble . SalaryRecord.salaryQ1
    teamCount = fromIntegral . length . Teams.toList . TeammateRecord.teamsQ1
    fst3 (p, _, _) = p

joinBudgetRecordsQ1 :: BudgetRecords.T -> [[RecordTuple]]
joinBudgetRecordsQ1
  BudgetRecords.T
    { BudgetRecords.priorityRecords = priorities,
      BudgetRecords.salaryRecords = salaries,
      BudgetRecords.teammateRecords = teammates
    } = groupBy samePriority $ filterQ1 recordTuples
    where
      samePriority (p1, _, _) (p2, _, _) = priorityQ1 p1 == priorityQ1 p2
      filterQ1 = filter joinedInQ1
      recordTuples = cartesianProduct priorities salaries teammates
      priorityQ1 = PriorityRecord.priorityQ1

joinedInQ1 :: RecordTuple -> Bool
joinedInQ1 (priority, salary, teammate) =
  PriorityRecord.team priority `elem` Teams.toList (TeammateRecord.teamsQ1 teammate)
    && TeammateRecord.bhc teammate == SalaryRecord.bhc salary

cartesianProduct :: [a] -> [b] -> [c] -> [(a, b, c)]
cartesianProduct = Control.Applicative.liftA3 (,,)
