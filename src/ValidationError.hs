module ValidationError (T (..), LineNumber (..), toText) where

import qualified Bhc
import Protolude
import qualified Team

data T
  = DuplicateTeamInPriorities LineNumber Team.T
  | DuplicateBhcInSalaries LineNumber Bhc.T
  | DuplicateBhcInTeammates LineNumber Bhc.T
  | MissingTeamInPriorities LineNumber Team.T
  | MissingBhcInSalaries LineNumber Bhc.T
  | MissingBhcInTeammates LineNumber Bhc.T
  | MissingTeamInTeammates LineNumber Team.T

newtype LineNumber = LineNumber Int deriving newtype (Num)

toText :: T -> Text
toText (DuplicateTeamInPriorities (LineNumber line) team) =
  "Duplicate team \"" <> Team.toText team <> "\" found on line " <> show line <> " in priorities file"
toText (DuplicateBhcInSalaries (LineNumber line) bhc) =
  "Duplicate BHC \"" <> Bhc.toText bhc <> "\" found on line " <> show line <> " in salaries file"
toText (DuplicateBhcInTeammates (LineNumber line) bhc) =
  "Duplicate BHC \"" <> Bhc.toText bhc <> "\" found on line " <> show line <> " in teammates file"
toText (MissingTeamInPriorities (LineNumber line) team) =
  "Missing team \"" <> Team.toText team <> "\" in priorities file, found on line " <> show line <> " in teammates file"
toText (MissingBhcInSalaries (LineNumber line) bhc) =
  "Missing BHC \"" <> Bhc.toText bhc <> "\" in salaries file, found on line " <> show line <> " in teammates file"
toText (MissingBhcInTeammates (LineNumber line) bhc) =
  "Missing BHC \"" <> Bhc.toText bhc <> "\" in teammates file, found on line " <> show line <> " in salaries file"
toText (MissingTeamInTeammates (LineNumber line) team) =
  "Missing team \"" <> Team.toText team <> "\" in teammates file, found on line " <> show line <> " in priorities file"
