module PriorityRecord (PriorityRecord (..)) where

import Data.Csv ((.:), FromNamedRecord (parseNamedRecord))
import Protolude

data PriorityRecord
  = PriorityRecord
      { name :: !Text,
        priorityQ1 :: !Text,
        priorityQ2 :: !Text,
        priorityQ3 :: !Text,
        priorityQ4 :: !Text
      }
  deriving (Show, Eq)

instance FromNamedRecord PriorityRecord where
  parseNamedRecord m =
    PriorityRecord
      <$> m .: "Team"
      <*> m .: "Priority Q1"
      <*> m .: "Priority Q2"
      <*> m .: "Priority Q3"
      <*> m .: "Priority Q4"
