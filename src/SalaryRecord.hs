module SalaryRecord (T (..)) where

import qualified Bhc
import Data.Csv ((.:), FromNamedRecord (parseNamedRecord))
import qualified Name
import Protolude
import qualified Salary

data T
  = T
      { bhc :: !Bhc.T,
        name :: !Name.T,
        salaryQ1 :: !Salary.T,
        salaryQ2 :: !Salary.T,
        salaryQ3 :: !Salary.T,
        salaryQ4 :: !Salary.T
      }
  deriving (Show, Eq, Ord)

instance FromNamedRecord T where
  parseNamedRecord m =
    T
      <$> m .: "Bhc"
      <*> m .: "Name"
      <*> m .: "Salary Q1"
      <*> m .: "Salary Q2"
      <*> m .: "Salary Q3"
      <*> m .: "Salary Q4"
