module Validator (validate) where

import qualified Bhc
import qualified BudgetRecords
import qualified Data.Char
import qualified Data.Set as Set
import qualified Data.Text
import qualified PriorityRecord
import Protolude
import qualified SalaryRecord
import qualified Team
import qualified TeammateRecord
import qualified Teams
import qualified ValidationError

validate :: BudgetRecords.T -> [ValidationError.T]
validate
  BudgetRecords.T
    { BudgetRecords.priorityRecords,
      BudgetRecords.salaryRecords,
      BudgetRecords.teammateRecords
    } =
    concat
      [ blankSalaryBhcErrors salaryRecords,
        blankTeammateBhcErrors teammateRecords,
        duplicateTeamInPrioritiesErrors priorityRecords,
        duplicateBhcsInSalariesErrors salaryRecords,
        duplicateBhcsInTeammatesErrors teammateRecords,
        missingTeamInPrioritiesErrors teammateRecords priorityRecords,
        missingBhcInSalariesErrors teammateRecords salaryRecords,
        missingBhcInTeammatesErrors salaryRecords teammateRecords
      ]

blankSalaryBhcErrors :: [SalaryRecord.T] -> [ValidationError.T]
blankSalaryBhcErrors salaries = map validationError blankSalaries
  where
    validationError salary = ValidationError.BlankSalaryBhcError (line salary)
    blankSalaries = filter (isBlank . Bhc.toText . SalaryRecord.bhc) salaries
    line = SalaryRecord.lineNumber

blankTeammateBhcErrors :: [TeammateRecord.T] -> [ValidationError.T]
blankTeammateBhcErrors teammates = map validationError blankTeammates
  where
    validationError teammate = ValidationError.BlankTeammateBhcError (line teammate)
    blankTeammates = filter (isBlank . Bhc.toText . TeammateRecord.bhc) teammates
    line = TeammateRecord.lineNumber

duplicateTeamInPrioritiesErrors :: [PriorityRecord.T] -> [ValidationError.T]
duplicateTeamInPrioritiesErrors priorities =
  map validationError duplicatePriorities
  where
    validationError priority =
      ValidationError.DuplicateTeamInPriorities (line priority) (PriorityRecord.team priority)
    duplicatePriorities = Set.toList $ prioritiesWithDuplicateTeams prioritySet
    prioritySet = Set.fromList priorities
    line = PriorityRecord.lineNumber

duplicateBhcsInSalariesErrors :: [SalaryRecord.T] -> [ValidationError.T]
duplicateBhcsInSalariesErrors salaries =
  map validationError duplicateSalaries
  where
    validationError salary =
      ValidationError.DuplicateBhcInSalaries (line salary) (SalaryRecord.bhc salary)
    duplicateSalaries = Set.toList $ salariesWithDuplicateBhcs salarySet
    salarySet = Set.fromList salaries
    line = SalaryRecord.lineNumber

duplicateBhcsInTeammatesErrors :: [TeammateRecord.T] -> [ValidationError.T]
duplicateBhcsInTeammatesErrors teammates =
  map validationError duplicateTeammates
  where
    validationError teammate = ValidationError.DuplicateBhcInTeammates (line teammate) (TeammateRecord.bhc teammate)
    duplicateTeammates = Set.toList $ teammatesWithDuplicateBhcs teammateSet
    teammateSet = Set.fromList teammates
    line = TeammateRecord.lineNumber

missingTeamInPrioritiesErrors :: [TeammateRecord.T] -> [PriorityRecord.T] -> [ValidationError.T]
missingTeamInPrioritiesErrors teammates priorities = do
  map validationError missingTeamsTeammates
  where
    validationError (team, teammate) = ValidationError.MissingTeamInPriorities (line teammate) team
    missingTeamsTeammates = Set.toList $ joinTeamsTeammatesOnTeam missingTeamSet teammateSet
    missingTeamSet = teamsMissingFromPriorities teammateSet prioritySet
    prioritySet = Set.fromList priorities
    teammateSet = Set.fromList teammates
    line = TeammateRecord.lineNumber

missingBhcInSalariesErrors :: [TeammateRecord.T] -> [SalaryRecord.T] -> [ValidationError.T]
missingBhcInSalariesErrors teammates salaries = do
  map validationError missingTeammates
  where
    validationError teammate = ValidationError.MissingBhcInSalaries (line teammate) (bhc teammate)
    missingTeammates = Set.toList $ teammatesMissingFromSalaries teammateSet salarySet
    teammateSet = Set.fromList teammates
    salarySet = Set.fromList salaries
    line = TeammateRecord.lineNumber
    bhc = TeammateRecord.bhc

missingBhcInTeammatesErrors :: [SalaryRecord.T] -> [TeammateRecord.T] -> [ValidationError.T]
missingBhcInTeammatesErrors salaries teammates = do
  map validationError missingSalaries
  where
    validationError salary = ValidationError.MissingBhcInTeammates (line salary) (bhc salary)
    missingSalaries = Set.toList $ salariesMissingFromTeammates salarySet teammateSet
    salarySet = Set.fromList salaries
    teammateSet = Set.fromList teammates
    line = SalaryRecord.lineNumber
    bhc = SalaryRecord.bhc

teammatesMissingFromSalaries :: Set TeammateRecord.T -> Set SalaryRecord.T -> Set TeammateRecord.T
teammatesMissingFromSalaries teammates salaries = do
  Set.difference teammates present
  where
    present = Set.map snd $ joinSalariesTeammatesOnBhc salaries teammates

salariesMissingFromTeammates :: Set SalaryRecord.T -> Set TeammateRecord.T -> Set SalaryRecord.T
salariesMissingFromTeammates salaries teammates = do
  Set.difference salaries present
  where
    present = Set.map fst $ joinSalariesTeammatesOnBhc salaries teammates

teamsMissingFromPriorities :: Set TeammateRecord.T -> Set PriorityRecord.T -> Set Team.T
teamsMissingFromPriorities teammates priorities =
  Set.difference (teamsFromTeammates teammates) (teamsFromPriorities priorities)

prioritiesWithDuplicateTeams :: Set PriorityRecord.T -> Set PriorityRecord.T
prioritiesWithDuplicateTeams priorities =
  Set.filter (\p -> priorityTeamOccurences p > 1) priorities
  where
    priorityTeamOccurences priority =
      Set.size $ Set.filter (\p -> PriorityRecord.team p == PriorityRecord.team priority) priorities

teammatesWithDuplicateBhcs :: Set TeammateRecord.T -> Set TeammateRecord.T
teammatesWithDuplicateBhcs teammates =
  Set.filter (\t -> teammateBhcOccurences t > 1) teammates
  where
    teammateBhcOccurences teammate =
      Set.size $ Set.filter (\t -> TeammateRecord.bhc t == TeammateRecord.bhc teammate) teammates

salariesWithDuplicateBhcs :: Set SalaryRecord.T -> Set SalaryRecord.T
salariesWithDuplicateBhcs salaries =
  Set.filter (\s -> salaryBhcOccurrences s > 1) salaries
  where
    salaryBhcOccurrences salary =
      Set.size $ Set.filter (\s -> SalaryRecord.bhc s == SalaryRecord.bhc salary) salaries

teamsFromTeammates :: Set TeammateRecord.T -> Set Team.T
teamsFromTeammates teammates = Set.unions $ Set.map teamsFromTeammate teammates

joinSalariesTeammatesOnBhc :: Set SalaryRecord.T -> Set TeammateRecord.T -> Set (SalaryRecord.T, TeammateRecord.T)
joinSalariesTeammatesOnBhc salaries teammates =
  Set.filter
    (\(salary, teammate) -> SalaryRecord.bhc salary == TeammateRecord.bhc teammate)
    (Set.cartesianProduct salaries teammates)

joinTeamsTeammatesOnTeam :: Set Team.T -> Set TeammateRecord.T -> Set (Team.T, TeammateRecord.T)
joinTeamsTeammatesOnTeam teams teammates =
  Set.filter
    (\(team, teammate) -> team `elem` teamsFromTeammate teammate)
    (Set.cartesianProduct teams teammates)

teamsFromTeammate :: TeammateRecord.T -> Set Team.T
teamsFromTeammate
  TeammateRecord.T
    { TeammateRecord.teamsQ1,
      TeammateRecord.teamsQ2,
      TeammateRecord.teamsQ3,
      TeammateRecord.teamsQ4
    } = Set.unions [toSet teamsQ1, toSet teamsQ2, toSet teamsQ3, toSet teamsQ4]
    where
      toSet teams = Set.fromList $ Teams.toList teams

teamsFromPriorities :: Set PriorityRecord.T -> Set Team.T
teamsFromPriorities = Set.map PriorityRecord.team

isBlank :: Text -> Bool
isBlank = Data.Text.foldl (\b c -> b && Data.Char.isSpace c) True
