module Validator (validate) where

import qualified Bhc
import qualified BudgetRecords
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
    catMaybes
      [ teammatesMissingFromSalariesErrors teammateRecords salaryRecords,
        salariesMissingFromTeammatesErrors salaryRecords teammateRecords,
        teamsMissingFromPrioritiesErrors teammateRecords priorityRecords,
        teamsMissingFromTeammatesErrors priorityRecords teammateRecords,
        duplicateTeamsInPrioritiesErrors priorityRecords,
        duplicateBhcsInTeammatesErrors teammateRecords,
        duplicateBhcsInSalariesErrors salaryRecords
      ]

teammatesMissingFromSalariesErrors :: [TeammateRecord.T] -> [SalaryRecord.T] -> Maybe ValidationError.T
teammatesMissingFromSalariesErrors teammates salaries = do
  if not $ null missingBhcs
    then Just $ validationError "BHCs found in teammates that are missing in salaries" missingBhcs
    else Nothing
  where
    missingBhcs = map (Bhc.toText . TeammateRecord.bhc) missingTeammates
    missingTeammates = Set.toList $ teammatesMissingFromSalaries teammateSet salarySet
    teammateSet = Set.fromList teammates
    salarySet = Set.fromList salaries

salariesMissingFromTeammatesErrors :: [SalaryRecord.T] -> [TeammateRecord.T] -> Maybe ValidationError.T
salariesMissingFromTeammatesErrors salaries teammates = do
  if not $ null missingBhcs
    then Just $ validationError "BHCs found in salaries that are missing in teammates" missingBhcs
    else Nothing
  where
    missingBhcs = map (Bhc.toText . SalaryRecord.bhc) missingSalaries
    missingSalaries = Set.toList $ salariesMissingFromTeammates salarySet teammateSet
    salarySet = Set.fromList salaries
    teammateSet = Set.fromList teammates

teamsMissingFromPrioritiesErrors :: [TeammateRecord.T] -> [PriorityRecord.T] -> Maybe ValidationError.T
teamsMissingFromPrioritiesErrors teammates priorities = do
  if not $ null missingTeams
    then Just $ validationError "Teams found in teammates that are missing from priorities" missingTeams
    else Nothing
  where
    missingTeams = map Team.toText missingTeamList
    missingTeamList = Set.toList $ teamsMissingFromPriorities teammateSet prioritySet
    teammateSet = Set.fromList teammates
    prioritySet = Set.fromList priorities

teamsMissingFromTeammatesErrors :: [PriorityRecord.T] -> [TeammateRecord.T] -> Maybe ValidationError.T
teamsMissingFromTeammatesErrors priorities teammates = do
  if not $ null missingTeams
    then Just $ validationError "Teams found in priorities that are missing from teammates" missingTeams
    else Nothing
  where
    missingTeams = map Team.toText missingTeamList
    missingTeamList = Set.toList $ teamsMissingFromTeammates prioritySet teammateSet
    teammateSet = Set.fromList teammates
    prioritySet = Set.fromList priorities

duplicateTeamsInPrioritiesErrors :: [PriorityRecord.T] -> Maybe ValidationError.T
duplicateTeamsInPrioritiesErrors priorities = do
  if not $ null duplicateTeams
    then Just $ validationError "Duplicate teams found in priorities" duplicateTeams
    else Nothing
  where
    duplicateTeams = map Team.toText duplicateTeamList
    duplicateTeamList = Set.toList $ duplicateTeamsInPriorities prioritySet
    prioritySet = Set.fromList priorities

duplicateBhcsInTeammatesErrors :: [TeammateRecord.T] -> Maybe ValidationError.T
duplicateBhcsInTeammatesErrors teammates = do
  if not $ null duplicateBhcs
    then Just $ validationError "Duplicate BHCs found in teammates" duplicateBhcs
    else Nothing
  where
    duplicateBhcs = map Bhc.toText duplicateBhcsList
    duplicateBhcsList = Set.toList $ duplicateBhcsInTeammates teammateSet
    teammateSet = Set.fromList teammates

duplicateBhcsInSalariesErrors :: [SalaryRecord.T] -> Maybe ValidationError.T
duplicateBhcsInSalariesErrors salaries = do
  if not $ null duplicateBhcs
    then Just $ validationError "Duplicate BHCs found in salaries" duplicateBhcs
    else Nothing
  where
    duplicateBhcs = map Bhc.toText duplicateBhcsList
    duplicateBhcsList = Set.toList $ duplicateBhcsInSalaries salarySet
    salarySet = Set.fromList salaries

validationError :: Text -> [Text] -> ValidationError.T
validationError message items =
  ValidationError.make $ message <> ": " <> Data.Text.intercalate ", " quotedItems
  where
    quotedItems = map (\i -> "\"" <> i <> "\"") items

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

teamsMissingFromTeammates :: Set PriorityRecord.T -> Set TeammateRecord.T -> Set Team.T
teamsMissingFromTeammates priorities teammates =
  Set.difference (teamsFromPriorities priorities) (teamsFromTeammates teammates)

duplicateTeamsInPriorities :: Set PriorityRecord.T -> Set Team.T
duplicateTeamsInPriorities priorities = Set.map PriorityRecord.team $ prioritiesWithDuplicateTeams priorities

duplicateBhcsInTeammates :: Set TeammateRecord.T -> Set Bhc.T
duplicateBhcsInTeammates teammates = Set.map TeammateRecord.bhc $ teammatesWithDuplicateBhcs teammates

duplicateBhcsInSalaries :: Set SalaryRecord.T -> Set Bhc.T
duplicateBhcsInSalaries salaries = Set.map SalaryRecord.bhc $ salariesWithDuplicateBhcs salaries

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

joinSalariesTeammatesOnBhc :: Set SalaryRecord.T -> Set TeammateRecord.T -> Set (SalaryRecord.T, TeammateRecord.T)
joinSalariesTeammatesOnBhc salaries teammates =
  Set.filter
    (\(s, t) -> SalaryRecord.bhc s == TeammateRecord.bhc t)
    (Set.cartesianProduct salaries teammates)
