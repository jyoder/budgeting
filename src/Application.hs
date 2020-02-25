module Application (run) where

import Actions
import Data.Csv (FromNamedRecord, decodeByName)
import PriorityRecord
import Protolude
import SalaryRecord
import TeammateRecord
import Prelude (String)

data BudgetRecords
  = BudgetRecords
      { priorityRecords :: [PriorityRecord],
        salaryRecords :: [SalaryRecord],
        teammateRecords :: [TeammateRecord]
      }
  deriving (Show)

data FileConfig
  = FileConfig
      { initiativeFile :: FilePath,
        salaryFile :: FilePath,
        teammateFile :: FilePath
      }
  deriving (Show)

newtype BudgetReport = BudgetReport String deriving (Show)

newtype ErrorMessage = ErrorMessage String deriving (Show)

run :: Monad m => Actions m -> m ()
run Actions {getArguments, readFileData, putString} = do
  arguments <- getArguments
  records <- getBudgetRecords readFileData arguments
  let report = either Left makeBudgetReport records
  either (putString . show) (printBudgetReport putString) report

getBudgetRecords :: Monad m => ReadFileData m -> Arguments -> m (Either ErrorMessage BudgetRecords)
getBudgetRecords readFileData args = either (return . Left) (loadBudgetRecords readFileData) $ parseArgs args

makeBudgetReport :: BudgetRecords -> Either ErrorMessage BudgetReport
makeBudgetReport _ = Left $ ErrorMessage "Not Implemented"

printBudgetReport :: PutString m -> BudgetReport -> m ()
printBudgetReport putString budgetReport = putString $ show budgetReport

parseArgs :: Arguments -> Either ErrorMessage FileConfig
parseArgs [initiativeFile, salaryFile, teammateFile] = Right $ FileConfig initiativeFile salaryFile teammateFile
parseArgs _ = Left $ ErrorMessage "Usage: ./budgeting-exe <initiative-csv> <salary-csv> <teammate-csv>"

loadBudgetRecords :: Monad m => ReadFileData m -> FileConfig -> m (Either ErrorMessage BudgetRecords)
loadBudgetRecords readFileData FileConfig {initiativeFile, salaryFile, teammateFile} = do
  initiativeRecords <- loadRecordsFromCsv readFileData initiativeFile
  salaryRecords <- loadRecordsFromCsv readFileData salaryFile
  teammateRecords <- loadRecordsFromCsv readFileData teammateFile
  return $ BudgetRecords <$> initiativeRecords <*> salaryRecords <*> teammateRecords

loadRecordsFromCsv :: Monad m => FromNamedRecord f => ReadFileData m -> FilePath -> m (Either ErrorMessage [f])
loadRecordsFromCsv readFileF filePath = do
  csvData <- readFileF filePath
  let decodedData = decodeByName =<< csvData
  return $ either (Left . ErrorMessage) (Right . toList . snd) decodedData
