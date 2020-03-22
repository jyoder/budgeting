module ValidatorSpec (spec) where

import qualified BudgetRecords
import qualified PriorityRecord
import Protolude
import qualified SalaryRecord
import qualified TeammateRecord
import qualified Teams
import Test.Hspec
import qualified ValidationError
import qualified Validator

spec :: Spec
spec = do
  describe "validate" $ do
    it "returns an empty list when there are no validation errors" $ do
      let ramsPriority = PriorityRecord.T 4 "Rams" "P1" "P2" "P3" "P4"
      let goatsPriority = PriorityRecord.T 1 "Goats" "P1" "P2" "P3" "P4"
      let jetsPriority = PriorityRecord.T 2 "Jets" "P1" "P2" "P3" "P4"
      let bobSalary = SalaryRecord.T 1 "123" "Bob" 100.0 100.0 100.0 100.0
      let robSalary = SalaryRecord.T 2 "124" "Rob" 101.0 101.0 101.0 101.0
      let patSalary = SalaryRecord.T 3 "125" "Pat" 102.0 102.0 102.0 102.0
      let bobTeammate = TeammateRecord.T 1 "123" "Bob" "Sports" rams rams rams rams
      let robTeammate = TeammateRecord.T 2 "124" "Rob" "Sports" goats goats goats goats
      let patTeammate = TeammateRecord.T 3 "125" "Pat" "Extreme Sports" goats goats goatsJets goatsJets
      let priorities = [goatsPriority, jetsPriority, ramsPriority]
      let salaries = [bobSalary, robSalary, patSalary]
      let teammates = [bobTeammate, robTeammate, patTeammate]
      let records = BudgetRecords.T priorities salaries teammates
      Validator.validate records `shouldBe` []
    it "returns an error when a salary has a blank BHC" $ do
      let records = BudgetRecords.T [] [SalaryRecord.T 1 "" "Bob" 100.0 100.0 100.0 100.0] []
      Validator.validate records `shouldContain` [ValidationError.BlankSalaryBhcError 1]
    it "returns an error when a teammate has a blank BHC" $ do
      let records = BudgetRecords.T [] [] [TeammateRecord.T 1 "" "Bob" "Sports" rams rams rams rams]
      Validator.validate records `shouldContain` [ValidationError.BlankTeammateBhcError 1]
    it "returns an error when a duplicate team is found in priorities" $ do
      let priority1 = PriorityRecord.T 1 "Goats" "P1" "P2" "P3" "P4"
      let priority2 = PriorityRecord.T 2 "Goats" "P5" "P6" "P7" "P8"
      let records = BudgetRecords.T [priority1, priority2] [] []
      Validator.validate records `shouldContain` [ValidationError.DuplicateTeamInPriorities 1 "Goats"]
    it "returns an error when a duplicate BHC is found in salaries" $ do
      let bobSalary = SalaryRecord.T 1 "123" "Bob" 100.0 100.0 100.0 100.0
      let robSalary = SalaryRecord.T 2 "123" "Rob" 101.0 101.0 101.0 101.0
      let records = BudgetRecords.T [] [bobSalary, robSalary] []
      Validator.validate records `shouldContain` [ValidationError.DuplicateBhcInSalaries 1 "123"]
      Validator.validate records `shouldContain` [ValidationError.DuplicateBhcInSalaries 2 "123"]
    it "returns an error when a duplicate BHC is found in teammates" $ do
      let bobTeammate = TeammateRecord.T 1 "123" "Bob" "Sports" rams rams rams rams
      let robTeammate = TeammateRecord.T 2 "123" "Rob" "Farming" goats goats goats goats
      let records = BudgetRecords.T [] [] [bobTeammate, robTeammate]
      Validator.validate records `shouldContain` [ValidationError.DuplicateBhcInTeammates 1 "123"]
      Validator.validate records `shouldContain` [ValidationError.DuplicateBhcInTeammates 2 "123"]
    it "returns an error when a team is found in teammates that is not present in priorities" $ do
      let goatsPriority = PriorityRecord.T 1 "Goats" "P1" "P2" "P3" "P4"
      let jetsPriority = PriorityRecord.T 2 "Jets" "P1" "P2" "P3" "P4"
      let bobSalary = SalaryRecord.T 1 "123" "Bob" 100.0 100.0 100.0 100.0
      let robSalary = SalaryRecord.T 2 "124" "Rob" 101.0 101.0 101.0 101.0
      let patSalary = SalaryRecord.T 3 "125" "Pat" 102.0 102.0 102.0 102.0
      let bobTeammate = TeammateRecord.T 1 "123" "Bob" "Sports" rams rams rams rams
      let robTeammate = TeammateRecord.T 2 "124" "Rob" "Sports" goats goats goats goats
      let patTeammate = TeammateRecord.T 3 "125" "Pat" "Extreme Sports" goats goats goatsJets goatsJets
      let priorities = [goatsPriority, jetsPriority]
      let salaries = [bobSalary, robSalary, patSalary]
      let teammates = [bobTeammate, robTeammate, patTeammate]
      let records = BudgetRecords.T priorities salaries teammates
      Validator.validate records `shouldMatchList` [ValidationError.MissingTeamInPriorities 1 "Rams"]
    it "returns an error when a team is found in teammates but there are no priorities" $ do
      let bobSalary = SalaryRecord.T 1 "124" "Bob" 100.0 100.0 100.0 100.0
      let robSalary = SalaryRecord.T 1 "125" "Rob" 101.0 101.0 101.0 101.0
      let bobTeammate = TeammateRecord.T 1 "123" "Bob" "Sports" rams rams rams rams
      let robTeammate = TeammateRecord.T 1 "124" "Rob" "Sports" goats goats goats goats
      let records = BudgetRecords.T [] [bobSalary, robSalary] [bobTeammate, robTeammate]
      Validator.validate records `shouldContain` [ValidationError.MissingTeamInPriorities 1 "Rams"]
    it "returns an error when a team is found in teammates that is not present in salaries" $ do
      let teammate = TeammateRecord.T 1 "123" "Bob" "Sports" goats goats goats goats
      let records = BudgetRecords.T [] [] [teammate]
      Validator.validate records `shouldContain` [ValidationError.MissingBhcInSalaries 1 "123"]
    it "returns an error when a team is found in salaries that is not present in teammates" $ do
      let priority = PriorityRecord.T 1 "Goats" "P1" "P2" "P3" "P4"
      let records = BudgetRecords.T [priority] [] []
      Validator.validate records `shouldContain` [ValidationError.MissingTeamInTeammates 1 "Goats"]
    it "returns multiple errors when several validations fail" $ do
      let ramsPriority = PriorityRecord.T 4 "Rams" "P1" "P2" "P3" "P4"
      let goatsPriority = PriorityRecord.T 1 "Goats" "P1" "P2" "P3" "P4"
      let jetsPriority = PriorityRecord.T 2 "Jets" "P1" "P2" "P3" "P4"
      let bobSalary = SalaryRecord.T 1 "123" "Bob" 100.0 100.0 100.0 100.0
      let robSalary = SalaryRecord.T 2 "123" "Rob" 101.0 101.0 101.0 101.0
      let patSalary = SalaryRecord.T 3 "" "Pat" 102.0 102.0 102.0 102.0
      let bobTeammate = TeammateRecord.T 1 "123" "Bob" "Sports" rams rams rams rams
      let robTeammate = TeammateRecord.T 2 "123" "Rob" "Sports" goats goats goats goats
      let patTeammate = TeammateRecord.T 3 "125" "Pat" "Extreme Sports" goats goats goats goats
      let priorities = [goatsPriority, goatsPriority, jetsPriority, ramsPriority]
      let salaries = [bobSalary, robSalary, patSalary]
      let teammates = [bobTeammate, robTeammate, patTeammate]
      let records = BudgetRecords.T priorities salaries teammates
      Validator.validate records
        `shouldMatchList` [ ValidationError.BlankSalaryBhcError 3,
                            ValidationError.DuplicateBhcInSalaries 1 "123",
                            ValidationError.DuplicateBhcInSalaries 2 "123",
                            ValidationError.DuplicateBhcInTeammates 1 "123",
                            ValidationError.DuplicateBhcInTeammates 2 "123",
                            ValidationError.MissingBhcInSalaries 3 "125",
                            ValidationError.MissingBhcInTeammates 3 "",
                            ValidationError.MissingTeamInTeammates 2 "Jets"
                          ]

rams :: Teams.T
rams = Teams.make ["Rams"]

goats :: Teams.T
goats = Teams.make ["Goats"]

goatsJets :: Teams.T
goatsJets = Teams.make ["Goats", "Jets"]
