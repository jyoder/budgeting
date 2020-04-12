module PriorityRecord (T (..), priority, allPriorities) where

import Data.Csv ((.:))
import qualified Data.Csv
import qualified LineNumber
import qualified Priority
import Protolude
import qualified Quarter
import qualified Team

data T
  = T
      { lineNumber :: !LineNumber.T,
        team :: !Team.T,
        priorityQ1 :: !Priority.T,
        priorityQ2 :: !Priority.T,
        priorityQ3 :: !Priority.T,
        priorityQ4 :: !Priority.T
      }
  deriving (Show, Eq, Ord)

instance Data.Csv.FromNamedRecord T where
  parseNamedRecord m =
    T
      <$> pure 0
      <*> m .: "Team"
      <*> m .: "Priority Q1"
      <*> m .: "Priority Q2"
      <*> m .: "Priority Q3"
      <*> m .: "Priority Q4"

priority :: Quarter.T -> T -> Priority.T
priority Quarter.Q1 = priorityQ1
priority Quarter.Q2 = priorityQ2
priority Quarter.Q3 = priorityQ3
priority Quarter.Q4 = priorityQ4

allPriorities :: T -> [Priority.T]
allPriorities record =
  [ priorityQ1 record,
    priorityQ2 record,
    priorityQ3 record,
    priorityQ4 record
  ]
