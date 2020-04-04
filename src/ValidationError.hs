module ValidationError (T (..), toText) where

import qualified Bhc
import qualified LineNumber
import Protolude
import qualified Team

data T
  = BlankSalaryBhcError LineNumber.T
  | BlankTeammateBhcError LineNumber.T
  | DuplicateTeamInPriorities LineNumber.T Team.T
  | DuplicateBhcInSalaries LineNumber.T Bhc.T
  | DuplicateBhcInTeammates LineNumber.T Bhc.T
  | MissingTeamInPriorities LineNumber.T Team.T
  | MissingBhcInSalaries LineNumber.T Bhc.T
  | MissingBhcInTeammates LineNumber.T Bhc.T
  deriving (Show, Eq)

toText :: T -> Text
toText (BlankSalaryBhcError lineNumber) =
  "Blank BHC found on line " <> LineNumber.toText lineNumber <> " in salary file"
toText (BlankTeammateBhcError lineNumber) =
  "Blank BHC found on line " <> LineNumber.toText lineNumber <> " in teammates file"
toText (DuplicateTeamInPriorities lineNumber team) =
  "Duplicate team \"" <> Team.toText team <> "\" found on line " <> LineNumber.toText lineNumber <> " in priorities file"
toText (DuplicateBhcInSalaries lineNumber bhc) =
  "Duplicate BHC \"" <> Bhc.toText bhc <> "\" found on line " <> LineNumber.toText lineNumber <> " in salaries file"
toText (DuplicateBhcInTeammates lineNumber bhc) =
  "Duplicate BHC \"" <> Bhc.toText bhc <> "\" found on line " <> LineNumber.toText lineNumber <> " in teammates file"
toText (MissingTeamInPriorities lineNumber team) =
  "Missing team \"" <> Team.toText team <> "\" in priorities file, found on line " <> LineNumber.toText lineNumber <> " in teammates file"
toText (MissingBhcInSalaries lineNumber bhc) =
  "Missing BHC \"" <> Bhc.toText bhc <> "\" in salaries file, found on line " <> LineNumber.toText lineNumber <> " in teammates file"
toText (MissingBhcInTeammates lineNumber bhc) =
  "Missing BHC \"" <> Bhc.toText bhc <> "\" in teammates file, found on line " <> LineNumber.toText lineNumber <> " in salaries file"
