module BudgetRecordsSpec (spec) where

import qualified BudgetRecords
import qualified PriorityRecord
import Protolude
import qualified SalaryRecord
import qualified TeammateRecord
import qualified Teams
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provide access to the fields in the record" $ do
      let priority = PriorityRecord.T 1 "Team" "P1" "P2" "P3" "P4"
      let salary = SalaryRecord.T 1 "Bhc" "Name" 1 2 3 4
      let teams = Teams.make ["T1", "T2"]
      let teammate = TeammateRecord.T 1 "Bhc" "Name" "Sports" teams teams teams teams
      let records = BudgetRecords.T [priority] [salary] [teammate]
      BudgetRecords.priorityRecords records `shouldBe` [priority]
      BudgetRecords.salaryRecords records `shouldBe` [salary]
      BudgetRecords.teammateRecords records `shouldBe` [teammate]
  describe "show" $ do
    it "converts budget records to text" $ do
      let priority = PriorityRecord.T 1 "Team" "P1" "P2" "P3" "P5"
          salary = SalaryRecord.T 1 "Bhc" "Name" 1 2 3 5
          teams = Teams.make ["T1", "T2"]
          teammate = TeammateRecord.T 1 "Bhc" "Name" "Sports" teams teams teams teams
          records = BudgetRecords.T [priority] [salary] [teammate]
       in show records `shouldBe` ("T {priorityRecords = [T {lineNumber = T 1, team = T \"Team\", priorityQ1 = T \"P1\", priorityQ2 = T \"P2\", priorityQ3 = T \"P3\", priorityQ4 = T \"P5\"}], salaryRecords = [T {lineNumber = T 1, bhc = T \"Bhc\", name = T \"Name\", salaryQ1 = T 1.0, salaryQ2 = T 2.0, salaryQ3 = T 3.0, salaryQ4 = T 5.0}], teammateRecords = [T {lineNumber = T 1, bhc = T \"Bhc\", name = T \"Name\", department = T \"Sports\", teamsQ1 = T [T \"T1\",T \"T2\"], teamsQ2 = T [T \"T1\",T \"T2\"], teamsQ3 = T [T \"T1\",T \"T2\"], teamsQ4 = T [T \"T1\",T \"T2\"]}]}" :: Text)
  describe "Eq" $ do
    it "tests whether budget records are equal" $ do
      let priority = PriorityRecord.T 1 "Team" "P1" "P2" "P3" "P4"
          salary = SalaryRecord.T 1 "Bhc" "Name" 1 2 3 4
          teams = Teams.make ["T1", "T2"]
          teammate = TeammateRecord.T 1 "Bhc" "Name" "Sports" teams teams teams teams
          records1 = BudgetRecords.T [priority] [salary] [teammate]
          records2 = BudgetRecords.T [priority] [salary] []
       in records1 `shouldNotBe` records2
