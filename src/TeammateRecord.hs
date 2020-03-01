module TeammateRecord (T (..)) where

import Data.Csv ((.:), FromNamedRecord (parseNamedRecord))
import Protolude

data T
  = T
      { name :: !Text,
        bhc :: !Text,
        teamsQ1 :: !Text,
        teamsQ2 :: !Text,
        teamsQ3 :: !Text,
        teamsQ4 :: !Text
      }
  deriving (Show, Eq)

instance FromNamedRecord T where
  parseNamedRecord m =
    T
      <$> m .: "Name"
      <*> m .: "Bhc"
      <*> m .: "Teams Q1"
      <*> m .: "Teams Q2"
      <*> m .: "Teams Q3"
      <*> m .: "Teams Q4"
