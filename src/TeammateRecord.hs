module TeammateRecord (TeammateRecord (..)) where

import Data.Csv ((.:), FromNamedRecord (parseNamedRecord))
import Protolude

data TeammateRecord
  = TeammateRecord
      { name :: !Text,
        bhc :: !Text,
        teamsQ1 :: !Text,
        teamsQ2 :: !Text,
        teamsQ3 :: !Text,
        teamsQ4 :: !Text
      }
  deriving (Show, Eq)

instance FromNamedRecord TeammateRecord where
  parseNamedRecord m =
    TeammateRecord
      <$> m .: "Name"
      <*> m .: "Bhc"
      <*> m .: "Teams Q1"
      <*> m .: "Teams Q2"
      <*> m .: "Teams Q3"
      <*> m .: "Teams Q4"
