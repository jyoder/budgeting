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
          goatsPriority = PriorityRecord.T 1 "Goats" "P1" "P2" "P3" "P4"
          extraPriority = PriorityRecord.T 5 "Extra" "P1" "P2" "P3" "P4"
          jetsPriority = PriorityRecord.T 2 "Jets" "P1" "P2" "P3" "P4"
          bobSalary = SalaryRecord.T 1 "123" "Bob" 100.0 100.0 100.0 100.0
          robSalary = SalaryRecord.T 2 "124" "Rob" 101.0 101.0 101.0 101.0
          patSalary = SalaryRecord.T 3 "125" "Pat" 102.0 102.0 102.0 102.0
          bobTeammate = TeammateRecord.T 1 "123" "Bob" "Sports" "Player" rams rams rams rams
          robTeammate = TeammateRecord.T 2 "124" "Rob" "Sports" "Player" goats goats goats goats
          patTeammate = TeammateRecord.T 3 "125" "Pat" "Extreme Sports" "Player" goats goats goatsJets goatsJets
          priorities = [goatsPriority, jetsPriority, ramsPriority, extraPriority]
          salaries = [bobSalary, robSalary, patSalary]
          teammates = [bobTeammate, robTeammate, patTeammate]
          records = BudgetRecords.T priorities salaries teammates
       in Validator.validate records `shouldBe` []
    it "returns an error when a salary has a blank BHC" $ do
      let records = BudgetRecords.T [] [SalaryRecord.T 1 "" "Bob" 100.0 100.0 100.0 100.0] []
       in Validator.validate records `shouldContain` [ValidationError.BlankSalaryBhcError 1]
    it "returns an error when a teammate has a blank BHC" $ do
      let records = BudgetRecords.T [] [] [TeammateRecord.T 1 "" "Bob" "Sports" "Player" rams rams rams rams]
       in Validator.validate records `shouldContain` [ValidationError.BlankTeammateBhcError 1]
    it "returns an error when a duplicate team is found in priorities" $ do
      let priority1 = PriorityRecord.T 1 "Goats" "P1" "P2" "P3" "P4"
          priority2 = PriorityRecord.T 2 "Goats" "P5" "P6" "P7" "P8"
          records = BudgetRecords.T [priority1, priority2] [] []
       in Validator.validate records `shouldContain` [ValidationError.DuplicateTeamInPriorities 1 "Goats"]
    it "returns an error when a duplicate BHC is found in salaries" $ do
      let bobSalary = SalaryRecord.T 1 "123" "Bob" 100.0 100.0 100.0 100.0
          robSalary = SalaryRecord.T 2 "123" "Rob" 101.0 101.0 101.0 101.0
          records = BudgetRecords.T [] [bobSalary, robSalary] []
       in Validator.validate records
            `shouldContain` [ ValidationError.DuplicateBhcInSalaries 1 "123",
                              ValidationError.DuplicateBhcInSalaries 2 "123"
                            ]
    it "returns an error when a duplicate BHC is found in teammates" $ do
      let bobTeammate = TeammateRecord.T 1 "123" "Bob" "Sports" "Player" rams rams rams rams
          robTeammate = TeammateRecord.T 2 "123" "Rob" "Farming" "Player" goats goats goats goats
          records = BudgetRecords.T [] [] [bobTeammate, robTeammate]
       in Validator.validate records
            `shouldContain` [ ValidationError.DuplicateBhcInTeammates 1 "123",
                              ValidationError.DuplicateBhcInTeammates 2 "123"
                            ]
    it "returns an error when a team is found in teammates that is not present in priorities" $ do
      let goatsPriority = PriorityRecord.T 1 "Goats" "P1" "P2" "P3" "P4"
          jetsPriority = PriorityRecord.T 2 "Jets" "P1" "P2" "P3" "P4"
          bobSalary = SalaryRecord.T 1 "123" "Bob" 100.0 100.0 100.0 100.0
          robSalary = SalaryRecord.T 2 "124" "Rob" 101.0 101.0 101.0 101.0
          patSalary = SalaryRecord.T 3 "125" "Pat" 102.0 102.0 102.0 102.0
          bobTeammate = TeammateRecord.T 1 "123" "Bob" "Sports" "Player" rams rams rams rams
          robTeammate = TeammateRecord.T 2 "124" "Rob" "Sports" "Player" goats goats goats goats
          patTeammate = TeammateRecord.T 3 "125" "Pat" "Extreme Sports" "Player" goats goats goatsJets goatsJets
          priorities = [goatsPriority, jetsPriority]
          salaries = [bobSalary, robSalary, patSalary]
          teammates = [bobTeammate, robTeammate, patTeammate]
          records = BudgetRecords.T priorities salaries teammates
       in Validator.validate records
            `shouldMatchList` [ ValidationError.MissingTeamInPriorities 1 "Rams"
                              ]
    it "returns an error when a team is found in teammates but there are no priorities" $ do
      let bobSalary = SalaryRecord.T 1 "124" "Bob" 100.0 100.0 100.0 100.0
          robSalary = SalaryRecord.T 1 "125" "Rob" 101.0 101.0 101.0 101.0
          bobTeammate = TeammateRecord.T 1 "123" "Bob" "Sports" "Player" rams rams rams rams
          robTeammate = TeammateRecord.T 1 "124" "Rob" "Sports" "Player" goats goats goats goats
          records = BudgetRecords.T [] [bobSalary, robSalary] [bobTeammate, robTeammate]
       in Validator.validate records
            `shouldContain` [ ValidationError.MissingTeamInPriorities 1 "Rams"
                            ]
    it "returns an error when a team is found in teammates that is not present in salaries" $ do
      let teammate = TeammateRecord.T 1 "123" "Bob" "Sports" "Player" goats goats goats goats
          records = BudgetRecords.T [] [] [teammate]
       in Validator.validate records `shouldContain` [ValidationError.MissingBhcInSalaries 1 "123"]
    it "returns multiple errors when several validations fail" $ do
      let ramsPriority = PriorityRecord.T 4 "Rams" "P1" "P2" "P3" "P4"
          goatsPriority = PriorityRecord.T 1 "Goats" "P1" "P2" "P3" "P4"
          jetsPriority = PriorityRecord.T 2 "Jets" "P1" "P2" "P3" "P4"
          bobSalary = SalaryRecord.T 1 "123" "Bob" 100.0 100.0 100.0 100.0
          robSalary = SalaryRecord.T 2 "123" "Rob" 101.0 101.0 101.0 101.0
          patSalary = SalaryRecord.T 3 "" "Pat" 102.0 102.0 102.0 102.0
          bobTeammate = TeammateRecord.T 1 "123" "Bob" "Sports" "Player" rams rams rams rams
          robTeammate = TeammateRecord.T 2 "123" "Rob" "Sports" "Player" goats goats goats goats
          patTeammate = TeammateRecord.T 3 "125" "Pat" "Extreme Sports" "Player" goats goats goats goats
          priorities = [goatsPriority, goatsPriority, jetsPriority, ramsPriority]
          salaries = [bobSalary, robSalary, patSalary]
          teammates = [bobTeammate, robTeammate, patTeammate]
          records = BudgetRecords.T priorities salaries teammates
       in Validator.validate records
            `shouldMatchList` [ ValidationError.BlankSalaryBhcError 3,
                                ValidationError.DuplicateBhcInSalaries 1 "123",
                                ValidationError.DuplicateBhcInSalaries 2 "123",
                                ValidationError.DuplicateBhcInTeammates 1 "123",
                                ValidationError.DuplicateBhcInTeammates 2 "123",
                                ValidationError.MissingBhcInSalaries 3 "125",
                                ValidationError.MissingBhcInTeammates 3 ""
                              ]

rams :: Teams.T
rams = Teams.make ["Rams"]

goats :: Teams.T
goats = Teams.make ["Goats"]

goatsJets :: Teams.T
goatsJets = Teams.make ["Goats", "Jets"]
