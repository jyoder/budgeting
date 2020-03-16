module PriorityRecord (T (..)) where

import Data.Csv ((.:))
import qualified Data.Csv
import qualified Priority
import Protolude
import qualified Team

data T
  = T
      { team :: !Team.T,
        priorityQ1 :: !Priority.T,
        priorityQ2 :: !Priority.T,
        priorityQ3 :: !Priority.T,
        priorityQ4 :: !Priority.T
      }
  deriving (Show, Eq, Ord)

instance Data.Csv.FromNamedRecord T where
  parseNamedRecord m =
    T
      <$> m .: "Name"
      <*> m .: "Priority Q1"
      <*> m .: "Priority Q2"
      <*> m .: "Priority Q3"
      <*> m .: "Priority Q4"
