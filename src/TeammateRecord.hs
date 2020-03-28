module TeammateRecord (T (..), teams) where

import qualified Bhc
import Data.Csv ((.:))
import qualified Data.Csv
import qualified Department
import qualified LineNumber
import qualified Name
import Protolude
import qualified Quarter
import qualified Teams

data T
  = T
      { lineNumber :: !LineNumber.T,
        bhc :: !Bhc.T,
        name :: !Name.T,
        department :: !Department.T,
        teamsQ1 :: !Teams.T,
        teamsQ2 :: !Teams.T,
        teamsQ3 :: !Teams.T,
        teamsQ4 :: !Teams.T
      }
  deriving (Show, Eq, Ord)

instance Data.Csv.FromNamedRecord T where
  parseNamedRecord m =
    T
      <$> pure 0
      <*> m .: "Bhc"
      <*> m .: "Name"
      <*> m .: "Department"
      <*> m .: "Teams Q1"
      <*> m .: "Teams Q2"
      <*> m .: "Teams Q3"
      <*> m .: "Teams Q4"

teams :: Quarter.T -> T -> Teams.T
teams Quarter.Q1 = teamsQ1
teams Quarter.Q2 = teamsQ2
teams Quarter.Q3 = teamsQ3
teams Quarter.Q4 = teamsQ4
