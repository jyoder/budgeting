module SalaryRecord (T (..)) where

import Data.Csv ((.:), FromNamedRecord (parseNamedRecord))
import Protolude

data T
  = T
      { bhc :: !Text,
        name :: !Text,
        salaryQ1 :: !Double,
        salaryQ2 :: !Double,
        salaryQ3 :: !Double,
        salaryQ4 :: !Double
      }
  deriving (Show, Eq)

instance FromNamedRecord T where
  parseNamedRecord m =
    T
      <$> m .: "Bhc"
      <*> m .: "Name"
      <*> m .: "Salary Q1"
      <*> m .: "Salary Q2"
      <*> m .: "Salary Q3"
      <*> m .: "Salary Q4"
