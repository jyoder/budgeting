module TeammateRecord (TeammateRecord (..)) where

import Data.Csv ((.:), FromNamedRecord (parseNamedRecord))
import Protolude

data TeammateRecord
  = TeammateRecord
      { bhcId :: !Text,
        firstName :: !Text,
        lastName :: !Text,
        team :: !Text
      }
  deriving (Show, Eq)

instance FromNamedRecord TeammateRecord where
  parseNamedRecord m =
    TeammateRecord
      <$> m .: "Budget hc#"
      <*> m .: "First Name"
      <*> m .: "Last Name"
      <*> m .: "Team"
