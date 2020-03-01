module PriorityRecord (T (..)) where

import Data.Csv ((.:), FromNamedRecord (parseNamedRecord))
import Protolude

data T
  = T
      { name :: !Text,
        priorityQ1 :: !Text,
        priorityQ2 :: !Text,
        priorityQ3 :: !Text,
        priorityQ4 :: !Text
      }
  deriving (Show, Eq)

instance FromNamedRecord T where
  parseNamedRecord m =
    T
      <$> m .: "Name"
      <*> m .: "Priority Q1"
      <*> m .: "Priority Q2"
      <*> m .: "Priority Q3"
      <*> m .: "Priority Q4"
