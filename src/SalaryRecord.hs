module SalaryRecord (T (..), salary) where

import qualified Bhc
import Data.Csv ((.:))
import qualified Data.Csv
import qualified LineNumber
import qualified Money
import qualified Name
import Protolude
import qualified Quarter

data T
  = T
      { lineNumber :: !LineNumber.T,
        bhc :: !Bhc.T,
        name :: !Name.T,
        salaryQ1 :: !Money.T,
        salaryQ2 :: !Money.T,
        salaryQ3 :: !Money.T,
        salaryQ4 :: !Money.T
      }
  deriving (Show, Eq, Ord)

instance Data.Csv.FromNamedRecord T where
  parseNamedRecord m =
    T
      <$> pure 0
      <*> m .: "Bhc"
      <*> m .: "Name"
      <*> m .: "Salary Q1"
      <*> m .: "Salary Q2"
      <*> m .: "Salary Q3"
      <*> m .: "Salary Q4"

salary :: Quarter.T -> T -> Money.T
salary Quarter.Q1 = salaryQ1
salary Quarter.Q2 = salaryQ2
salary Quarter.Q3 = salaryQ3
salary Quarter.Q4 = salaryQ4
