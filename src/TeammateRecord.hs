module TeammateRecord (T (..), teams, withDefaultTeam) where

import qualified Bhc
import Data.Csv ((.:))
import qualified Data.Csv
import qualified Department
import qualified LineNumber
import qualified Name
import Protolude
import qualified Quarter
import qualified Role
import qualified Team
import qualified Teams

data T
  = T
      { lineNumber :: !LineNumber.T,
        bhc :: !Bhc.T,
        name :: !Name.T,
        department :: !Department.T,
        function :: !Role.T,
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
      <*> m .: "Role"
      <*> m .: "Teams Q1"
      <*> m .: "Teams Q2"
      <*> m .: "Teams Q3"
      <*> m .: "Teams Q4"

teams :: Quarter.T -> T -> Teams.T
teams Quarter.Q1 = teamsQ1
teams Quarter.Q2 = teamsQ2
teams Quarter.Q3 = teamsQ3
teams Quarter.Q4 = teamsQ4

withDefaultTeam :: T -> T
withDefaultTeam teammate =
  teammate
    { teamsQ1 = orDefaultTeam $ teamsQ1 teammate,
      teamsQ2 = orDefaultTeam $ teamsQ2 teammate,
      teamsQ3 = orDefaultTeam $ teamsQ3 teammate,
      teamsQ4 = orDefaultTeam $ teamsQ4 teammate
    }

orDefaultTeam :: Teams.T -> Teams.T
orDefaultTeam ts =
  if null $ Teams.toList ts
    then Teams.make [defaultTeam]
    else ts

defaultTeam :: Team.T
defaultTeam = Team.make "None"
