module SalaryRecord (T (..)) where

import qualified Bhc
import Data.Csv ((.:))
import qualified Data.Csv
import qualified LineNumber
import qualified Name
import Protolude
import qualified Salary

data T
  = T
      { lineNumber :: !LineNumber.T,
        bhc :: !Bhc.T,
        name :: !Name.T,
        salaryQ1 :: !Salary.T,
        salaryQ2 :: !Salary.T,
        salaryQ3 :: !Salary.T,
        salaryQ4 :: !Salary.T
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
