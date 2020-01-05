module SalaryRecord (SalaryRecord (..)) where

import Data.Csv ((.:), FromNamedRecord (parseNamedRecord))
import Protolude

data SalaryRecord
  = SalaryRecord
      { bhcId :: !Text,
        name :: !Text,
        salary :: !Double
      }
  deriving (Show, Eq)

instance FromNamedRecord SalaryRecord where
  parseNamedRecord m =
    SalaryRecord
      <$> m .: "Budget hc#"
      <*> m .: "Last name, First name"
      <*> m .: "Total Annualized Comp @ Target"
