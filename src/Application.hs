module Application (run) where

import qualified Actions
import qualified Argument
import qualified BudgetRecords
import qualified BudgetReport
import qualified Control.Monad.Except as E
import qualified CostCalculator
import qualified Csv
import qualified Data.Text
import qualified Error
import qualified FileConfig
import qualified Path
import qualified Preprocessor
import Protolude
import qualified ReportGenerator
import qualified Result
import qualified ValidationError
import qualified Validator

type App m a = E.ExceptT Error.T m a

run :: Monad m => Actions.T m -> m ()
run actions = do
  result <- runExceptT (run' actions)
  either (Actions.print actions . Error.toText) return result

run' :: Monad m => Actions.T m -> App m ()
run' actions = do
  arguments <- lift $ Actions.getArguments actions
  fileConfig <- E.liftEither $ parseArguments arguments
  records <- loadBudgetRecords (Actions.read actions) fileConfig
  let preprocessed = Preprocessor.preprocess records
  let errors = Validator.validate preprocessed
  if null errors
    then
      let report = ReportGenerator.generate CostCalculator.cost preprocessed
       in lift $ Actions.print actions $ BudgetReport.toCsv report
    else lift $ Actions.print actions $ Data.Text.unlines (map ValidationError.toText errors)

parseArguments :: [Argument.T] -> Result.T FileConfig.T
parseArguments arguments = case argumentToPath <$> arguments of
  [priorityPath, salaryPath, teammatePath] ->
    Result.success $ FileConfig.T priorityPath salaryPath teammatePath
  _ ->
    Result.error "Usage: ./budgeting-exe <priorities-csv> <salaries-csv> <teammates-csv>"

argumentToPath :: Argument.T -> Path.T
argumentToPath = Path.fromText . Argument.toText

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
