module Application (run) where

import qualified Actions
import qualified Argument
import qualified BudgetRecords
import qualified BudgetReport
import qualified BudgetReportGenerator
import qualified Control.Monad.Except as E
import qualified CostCalculator
import qualified Csv
import qualified Data.Text
import qualified Error
import qualified FileConfig
import qualified Path
import qualified Preprocessor
import Protolude
import qualified RatioReport
import qualified Result
import qualified ValidationError
import qualified Validator

type App m a = E.ExceptT Error.T m a

data Command = Budget | Ratios

run :: Monad m => Actions.T m -> m ()
run actions = do
  result <- runExceptT (run' actions)
  either (Actions.print actions . Error.toText) return result

run' :: Monad m => Actions.T m -> App m ()
run' actions = do
  arguments <- lift $ Actions.getArguments actions
  (command, fileConfig) <- E.liftEither $ parseArguments arguments
  records <- loadBudgetRecords (Actions.read actions) fileConfig
  let preprocessedRecords = Preprocessor.preprocess records
  let errors = Validator.validate preprocessedRecords
  if null errors
    then runCommand actions preprocessedRecords command
    else printErrors actions errors

parseArguments :: [Argument.T] -> Result.T (Command, FileConfig.T)
parseArguments [arg1, arg2, arg3, arg4] =
  let command = Argument.toText arg1
      priorityPath = argumentToPath arg2
      salaryPath = argumentToPath arg3
      teammatePath = argumentToPath arg4
      fileConfig = FileConfig.T priorityPath salaryPath teammatePath
   in case command of
        "budget" -> Result.success (Budget, fileConfig)
        "ratios" -> Result.success (Ratios, fileConfig)
        _ -> Result.error usageMessage
parseArguments _ =
  Result.error usageMessage

loadBudgetRecords :: Monad m => (Path.T -> App m Text) -> FileConfig.T -> App m BudgetRecords.T
loadBudgetRecords read FileConfig.T {FileConfig.prioritiesFile, FileConfig.salariesFile, FileConfig.teammatesFile} = do
  priorityRecords <- loadRecords read prioritiesFile
  salaryRecords <- loadRecords read salariesFile
  teammateRecords <- loadRecords read teammatesFile
  return $ BudgetRecords.T priorityRecords salaryRecords teammateRecords

loadRecords :: Monad m => Csv.FromNamedRecord f => (Path.T -> App m Text) -> Path.T -> App m [f]
loadRecords read filePath = do
  csvData <- read filePath
  E.liftEither $ either prependPath Result.success (Csv.decode csvData)
  where
    prependPath = Result.prepend $ Path.toText filePath <> ": "

runCommand :: Monad m => Actions.T m -> BudgetRecords.T -> Command -> App m ()
runCommand actions budgetRecords command =
  case command of
    Budget -> runBudgetCommand actions budgetRecords
    Ratios -> runRatiosCommand actions budgetRecords

runBudgetCommand :: Monad m => Actions.T m -> BudgetRecords.T -> App m ()
runBudgetCommand actions budgetRecords =
  let report = BudgetReportGenerator.generate CostCalculator.cost budgetRecords
   in lift $ Actions.print actions $ BudgetReport.toCsv report

runRatiosCommand :: Monad m => Actions.T m -> BudgetRecords.T -> App m ()
runRatiosCommand actions _ = lift $ Actions.print actions (RatioReport.toCsv $ RatioReport.T [RatioReport.Row "UX" 2.0 2.0 2.0 2.0])

printErrors :: Monad m => Actions.T m -> [ValidationError.T] -> App m ()
printErrors actions errors =
  lift $ Actions.print actions $ Data.Text.unlines (map ValidationError.toText errors)

argumentToPath :: Argument.T -> Path.T
argumentToPath = Path.fromText . Argument.toText

usageMessage :: Text
usageMessage = "Usage: ./budgeting-exe <budget|ratios> <priorities-csv> <salaries-csv> <teammates-csv>"
