module ReportGenerator (generate) where

import qualified Bhc
import qualified BudgetRecords
import qualified BudgetReport
import qualified Data.List
import qualified Data.Map as Map
import qualified Money
import qualified Priority
import qualified PriorityRecord
import Protolude
import qualified Quarter
import qualified SalaryRecord
import qualified Team
import qualified TeammateRecord
import qualified Teams
import qualified Unsafe

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

spendByQuarter ::
  (Money.T -> Money.T) ->
  Quarter.T ->
  BudgetRecords.T ->
  [(Priority.T, Money.T)]
spendByQuarter cost quarter budgetRecords = Map.toAscList spend
  where
    spend = Map.union computedSpendMap zeroedSpendMap
    computedSpendMap = spendByPriority cost quarter budgetRecords
    zeroedSpendMap = zeroSpendByPriority priorityRecords
    priorityRecords = BudgetRecords.priorityRecords budgetRecords

spendByPriority ::
  (Money.T -> Money.T) ->
  Quarter.T ->
  BudgetRecords.T ->
  Map Priority.T Money.T
spendByPriority
  cost
  quarter
  budgetRecords = Map.fromList spendList
    where
      spendList = map (spendAmount cost quarter) groupedTuples
      groupedTuples = tupleGroups quarter (recordTuples quarter budgetRecords)

spendAmount ::
  (Money.T -> Money.T) ->
  Quarter.T ->
  (Priority.T, [RecordTuple]) ->
  (Priority.T, Money.T)
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
tupleGroups quarter tuples = map (tupleGroup quarter) (groupByPriority sortedTuples)
  where
    groupByPriority = groupBy samePriority
    sortedTuples = sortBy compareTuples tuples
    samePriority (p1, _, _) (p2, _, _) = priority p1 == priority p2
    compareTuples (p1, _, _) (p2, _, _) = priority p1 `compare` priority p2
    priority = PriorityRecord.priority quarter

tupleGroup :: Quarter.T -> [RecordTuple] -> (Priority.T, [RecordTuple])
tupleGroup quarter tuples = (priority, tuples)
  where
    priority = PriorityRecord.priority quarter $ fst3 firstTuple
    firstTuple = Unsafe.unsafeHead tuples -- guaranteed not to be empty within this module
    fst3 (p, _, _) = p

recordTuples :: Quarter.T -> BudgetRecords.T -> [RecordTuple]
recordTuples
  quarter
  BudgetRecords.T
    { BudgetRecords.priorityRecords = priorities,
      BudgetRecords.salaryRecords = salaries,
      BudgetRecords.teammateRecords = teammates
    } = withPriorities priorities . withTeams quarter $ withTeammates salaries teammates

withTeammates ::
  [SalaryRecord.T] ->
  [TeammateRecord.T] ->
  [(SalaryRecord.T, TeammateRecord.T)]
withTeammates salaries teammates =
  Map.elems $ Map.intersectionWithKey pair salaryMap teammateMap
  where
    pair _ salary teammate = (salary, teammate)
    salaryMap = salariesByBhc salaries
    teammateMap = teammatesByBhc teammates

withTeams ::
  Quarter.T ->
  [(SalaryRecord.T, TeammateRecord.T)] ->
  [(SalaryRecord.T, TeammateRecord.T, Team.T)]
withTeams quarter = concatMap triples
  where
    triples (salary, teammate) = map (salary,teammate,) (teams teammate)
    teams teammate = Teams.toList $ TeammateRecord.teams quarter teammate

withPriorities ::
  [PriorityRecord.T] ->
  [(SalaryRecord.T, TeammateRecord.T, Team.T)] ->
  [RecordTuple]
withPriorities priorities = mapMaybe recordTuple
  where
    recordTuple (salary, teammate, team) = (,salary,teammate) <$> Map.lookup team priorityMap
    priorityMap = prioritiesByTeam priorities

salariesByBhc :: [SalaryRecord.T] -> Map Bhc.T SalaryRecord.T
salariesByBhc salaries = Map.fromList (map (\s -> (SalaryRecord.bhc s, s)) salaries)

prioritiesByTeam :: [PriorityRecord.T] -> Map Team.T PriorityRecord.T
prioritiesByTeam priorities = Map.fromList (map (\p -> (PriorityRecord.team p, p)) priorities)

teammatesByBhc :: [TeammateRecord.T] -> Map Bhc.T TeammateRecord.T
teammatesByBhc teammates = Map.fromList (map (\t -> (TeammateRecord.bhc t, t)) teammates)
