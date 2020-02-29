module SalaryRecord (SalaryRecord (..)) where

import Data.Csv ((.:), FromNamedRecord (parseNamedRecord))
import Protolude

data SalaryRecord
  = SalaryRecord
      { bhc :: !Text,
        name :: !Text,
        salaryQ1 :: !Double,
        salaryQ2 :: !Double,
        salaryQ3 :: !Double,
        salaryQ4 :: !Double
      }
  deriving (Show, Eq)

instance FromNamedRecord SalaryRecord where
  parseNamedRecord m =
    SalaryRecord
      <$> m .: "Bhc"
      <*> m .: "Name"
      <*> m .: "Salary Q1"
      <*> m .: "Salary Q2"
      <*> m .: "Salary Q3"
      <*> m .: "Salary Q4"
