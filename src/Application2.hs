module Application2 (run) where

import qualified Actions2
import qualified Argument
import qualified BudgetRecords
import qualified Control.Monad.Except as E
import qualified Csv
import qualified Error
import qualified FileConfig
import qualified Path
import Protolude
import qualified Result

type App a = E.ExceptT Error.T IO a

run :: Actions2.T -> IO ()
run actions = do
  result <- runExceptT (_run actions)
  either (Actions2.print actions . Error.toText) return result

_run :: Actions2.T -> App ()
_run actions = do
  arguments <- lift $ Actions2.getArguments actions
  fileConfig <- E.liftEither $ _parseArguments arguments
  records <- _loadBudgetRecords (Actions2.read actions) fileConfig
  lift $ Actions2.print actions $ show records

_parseArguments :: [Argument.T] -> Result.T FileConfig.T
_parseArguments arguments = case _argumentToPath <$> arguments of
  [priorityPath, salaryPath, teammatePath] ->
    Result.success $ FileConfig.T priorityPath salaryPath teammatePath
  _ ->
    Result.error "Usage: ./budgeting-exe <priorities-csv> <salaries-csv> <teammates-csv>"

_argumentToPath :: Argument.T -> Path.T
_argumentToPath = Path.fromText . Argument.toText

_loadBudgetRecords :: (Path.T -> App Text) -> FileConfig.T -> App BudgetRecords.T
_loadBudgetRecords readF FileConfig.T {FileConfig.priorityFile, FileConfig.salaryFile, FileConfig.teammateFile} = do
  priorityRecords <- _loadRecords readF priorityFile
  salaryRecords <- _loadRecords readF salaryFile
  teammateRecords <- _loadRecords readF teammateFile
  return $ BudgetRecords.T priorityRecords salaryRecords teammateRecords

_loadRecords :: Csv.FromNamedRecord f => (Path.T -> App Text) -> Path.T -> App [f]
_loadRecords readF filePath = do
  csvData <- readF filePath
  E.liftEither $ Csv.decode csvData
