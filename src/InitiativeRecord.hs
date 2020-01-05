module InitiativeRecord (InitiativeRecord (..)) where

import Data.Csv ((.:), FromNamedRecord (parseNamedRecord))
import Protolude

data InitiativeRecord
  = InitiativeRecord
      { name :: !Text,
        initiative :: !Text
      }
  deriving (Show, Eq)

instance FromNamedRecord InitiativeRecord where
  parseNamedRecord m =
    InitiativeRecord
      <$> m .: "Team"
      <*> m .: "Initiative"
