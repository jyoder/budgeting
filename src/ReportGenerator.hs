module ReportGenerator (generate) where

import qualified BudgetRecords
import qualified BudgetReport
import qualified Control.Applicative
import qualified Data.List
import qualified Data.Map as Map
import qualified Money
import qualified Priority
import qualified PriorityRecord
import Protolude
import qualified Quarter
import qualified SalaryRecord
import qualified TeammateRecord
import qualified Teams

type RecordTuple = (PriorityRecord.T, SalaryRecord.T, TeammateRecord.T)

generate :: (Money.T -> Money.T) -> BudgetRecords.T -> BudgetReport.T
generate cost budgetRecords = BudgetReport.T rows
  where
    rows = Data.List.zipWith4 makeRow spendQ1 spendQ2 spendQ3 spendQ4
    spendQ1 = spendByQuarter cost Quarter.Q1 budgetRecords
    spendQ2 = spendByQuarter cost Quarter.Q2 budgetRecords
    spendQ3 = spendByQuarter cost Quarter.Q3 budgetRecords
    spendQ4 = spendByQuarter cost Quarter.Q4 budgetRecords

makeRow ::
  (Priority.T, Money.T) ->
  (Priority.T, Money.T) ->
  (Priority.T, Money.T) ->
  (Priority.T, Money.T) ->
  BudgetReport.Row
makeRow (priority, spendQ1) (_, spendQ2) (_, spendQ3) (_, spendQ4) =
  BudgetReport.Row priority spendQ1 spendQ2 spendQ3 spendQ4

spendByQuarter :: (Money.T -> Money.T) -> Quarter.T -> BudgetRecords.T -> [(Priority.T, Money.T)]
spendByQuarter cost quarter budgetRecords = Map.toAscList spend
  where
    spend = Map.union computedSpendMap zeroedSpendMap
    computedSpendMap = spendByPriority cost quarter budgetRecords
    zeroedSpendMap = zeroSpendByPriority priorityRecords
    priorityRecords = BudgetRecords.priorityRecords budgetRecords

spendByPriority :: (Money.T -> Money.T) -> Quarter.T -> BudgetRecords.T -> Map Priority.T Money.T
spendByPriority
  cost
  quarter
  BudgetRecords.T
    { BudgetRecords.priorityRecords = priorities,
      BudgetRecords.salaryRecords = salaries,
      BudgetRecords.teammateRecords = teammates
    } = Map.fromList spendList
    where
      spendList = map (spendAmount cost quarter) groupedTuples
      groupedTuples = tupleGroups quarter filteredTuples
      filteredTuples = filter (validTuple quarter) tuples
      tuples = cartesianProduct priorities salaries teammates

spendAmount :: (Money.T -> Money.T) -> Quarter.T -> (Priority.T, [RecordTuple]) -> (Priority.T, Money.T)
spendAmount cost quarter (priority, tuples) = (priority, cost $ sum $ splitSalaries tuples)
  where
    splitSalaries = map splitSalary
    splitSalary (_, salary, teammate) = salaryInQuarter salary / fromIntegral (teamCount teammate)
    salaryInQuarter = SalaryRecord.salary quarter
    teamCount = length . Teams.toList . TeammateRecord.teams quarter

zeroSpendByPriority :: [PriorityRecord.T] -> Map Priority.T Money.T
zeroSpendByPriority priorities = Map.fromList $ map (,0.00) allPriorities
  where
    allPriorities = concatMap PriorityRecord.allPriorities priorities

tupleGroups :: Quarter.T -> [RecordTuple] -> [(Priority.T, [RecordTuple])]
tupleGroups quarter tuples = map (tupleGroup quarter) (groupByPriority tuples)
  where
    groupByPriority = groupBy samePriority
    samePriority (p1, _, _) (p2, _, _) = priority p1 == priority p2
    priority = PriorityRecord.priority quarter

tupleGroup :: Quarter.T -> [RecordTuple] -> (Priority.T, [RecordTuple])
tupleGroup _ [] = ("Undefined", [])
tupleGroup quarter (t : ts) = (priority, tuples)
  where
    tuples = t : ts
    priority = PriorityRecord.priority quarter $ fst3 t
    fst3 (p, _, _) = p

validTuple :: Quarter.T -> RecordTuple -> Bool
validTuple quarter (priority, salary, teammate) =
  PriorityRecord.team priority `elem` Teams.toList (TeammateRecord.teams quarter teammate)
    && TeammateRecord.bhc teammate == SalaryRecord.bhc salary

cartesianProduct :: [a] -> [b] -> [c] -> [(a, b, c)]
cartesianProduct = Control.Applicative.liftA3 (,,)
