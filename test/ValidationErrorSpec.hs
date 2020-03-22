module ValidationErrorSpec (spec) where

import Protolude
import Test.Hspec
import qualified ValidationError

spec :: Spec
spec = do
  describe "toText" $ do
    it "converts BlankSalaryBhcError to text" $ do
      ValidationError.toText (ValidationError.BlankSalaryBhcError 1)
        `shouldBe` "Blank BHC found on line 1 in salary file"
    it "converts BlankTeammateBhcError to text" $ do
      ValidationError.toText (ValidationError.BlankTeammateBhcError 1)
        `shouldBe` "Blank BHC found on line 1 in teammates file"
    it "converts DuplicateTeamInPriorities to text" $ do
      ValidationError.toText (ValidationError.DuplicateTeamInPriorities 1 "49ers")
        `shouldBe` "Duplicate team \"49ers\" found on line 1 in priorities file"
    it "converts DuplicateBhcInSalaries to text" $ do
      ValidationError.toText (ValidationError.DuplicateBhcInSalaries 1 "123")
        `shouldBe` "Duplicate BHC \"123\" found on line 1 in salaries file"
    it "converts DuplicateBhcInTeammates to text" $ do
      ValidationError.toText (ValidationError.DuplicateBhcInTeammates 1 "123")
        `shouldBe` "Duplicate BHC \"123\" found on line 1 in teammates file"
    it "converts MissingBhcInSalaries to text" $ do
      ValidationError.toText (ValidationError.MissingTeamInPriorities 1 "49ers")
        `shouldBe` "Missing team \"49ers\" in priorities file, found on line 1 in teammates file"
    it "converts MissingBhcInSalaries to text" $ do
      ValidationError.toText (ValidationError.MissingBhcInSalaries 1 "123")
        `shouldBe` "Missing BHC \"123\" in salaries file, found on line 1 in teammates file"
    it "converts MissingBhcInSalaries to text" $ do
      ValidationError.toText (ValidationError.MissingBhcInTeammates 1 "123")
        `shouldBe` "Missing BHC \"123\" in teammates file, found on line 1 in salaries file"
    it "converts MissingTeamInTeammates to text" $ do
      ValidationError.toText (ValidationError.MissingTeamInTeammates 1 "49ers")
        `shouldBe` "Missing team \"49ers\" in teammates file, found on line 1 in priorities file"
