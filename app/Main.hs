module Main where

import BudgetRecords
import Control.Exception (try)
import qualified Data.ByteString.Lazy as B
import Data.Csv (FromNamedRecord, decodeByName)
import Protolude
import System.Environment (getArgs)
import Prelude (String)

data FileConfig
  = FileConfig
      { initiativeFile :: FilePath,
        salaryFile :: FilePath,
        teammateFile :: FilePath
      }
  deriving (Show)

newtype BudgetReport = BudgetReport String deriving (Show)

newtype ErrorMessage = ErrorMessage String deriving (Show)

main :: IO ()
main = do
  records <- getBudgetRecords
  let report = either Left makeBudgetReport records
  either print printBudgetReport report

getBudgetRecords :: IO (Either ErrorMessage BudgetRecords)
getBudgetRecords = getArgs >>= either (return . Left) loadBudgetRecords . parseArgs

parseArgs :: [String] -> Either ErrorMessage FileConfig
parseArgs [initiativeFile, salaryFile, teammateFile] = Right $ FileConfig initiativeFile salaryFile teammateFile
parseArgs _ = Left $ ErrorMessage "Usage: ./budgeting-exe <initiative-csv> <salary-csv> <teammate-csv>"

loadBudgetRecords :: FileConfig -> IO (Either ErrorMessage BudgetRecords)
loadBudgetRecords FileConfig {initiativeFile, salaryFile, teammateFile} = do
  initiativeRecords <- loadRecordsFromCsv initiativeFile
  salaryRecords <- loadRecordsFromCsv salaryFile
  teammateRecords <- loadRecordsFromCsv teammateFile
  return $ BudgetRecords <$> initiativeRecords <*> salaryRecords <*> teammateRecords

makeBudgetReport :: BudgetRecords -> Either ErrorMessage BudgetReport
makeBudgetReport _ = Left $ ErrorMessage "Not Implemented"

printBudgetReport :: BudgetReport -> IO ()
printBudgetReport = print

loadRecordsFromCsv :: FromNamedRecord r => FilePath -> IO (Either ErrorMessage [r])
loadRecordsFromCsv filePath = do
  csvData <- safeReadFile filePath
  let decodedData = decodeByName =<< csvData
  return $ either (Left . ErrorMessage) (Right . toList . snd) decodedData

safeReadFile :: FilePath -> IO (Either String B.ByteString)
safeReadFile filePath = do
  fileData <- try $ B.readFile filePath :: IO (Either IOException B.ByteString)
  return $ either (Left . show) Right fileData
