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
    it "provides access to the fields in the record" $ do
      let (priority, salary, teams, teammate, record) =
            ( PriorityRecord.T 1 "Team" "P1" "P2" "P3" "P4",
              SalaryRecord.T 1 "Bhc" "Name" 1 2 3 4,
              Teams.make ["T1", "T2"],
              TeammateRecord.T 1 "Bhc" "Name" "Sports" teams teams teams teams,
              BudgetRecords.T [priority] [salary] [teammate]
            )
      BudgetRecords.priorityRecords record `shouldBe` [priority]
      BudgetRecords.salaryRecords record `shouldBe` [salary]
      BudgetRecords.teammateRecords record `shouldBe` [teammate]
  describe "show" $ do
    it "converts a budget record to text" $ do
      let (priority, salary, teams, teammate, record) =
            ( PriorityRecord.T 1 "Team" "P1" "P2" "P3" "P4",
              SalaryRecord.T 1 "Bhc" "Name" 1 2 3 4,
              Teams.make ["T1", "T2"],
              TeammateRecord.T 1 "Bhc" "Name" "Sports" teams teams teams teams,
              BudgetRecords.T [priority] [salary] [teammate]
            )
      show record `shouldBe` ("T {priorityRecords = [T {lineNumber = T 1, team = T \"Team\", priorityQ1 = T \"P1\", priorityQ2 = T \"P2\", priorityQ3 = T \"P3\", priorityQ4 = T \"P4\"}], salaryRecords = [T {lineNumber = T 1, bhc = T \"Bhc\", name = T \"Name\", salaryQ1 = T 1.0, salaryQ2 = T 2.0, salaryQ3 = T 3.0, salaryQ4 = T 4.0}], teammateRecords = [T {lineNumber = T 1, bhc = T \"Bhc\", name = T \"Name\", department = T \"Sports\", teamsQ1 = T [T \"T1\",T \"T2\"], teamsQ2 = T [T \"T1\",T \"T2\"], teamsQ3 = T [T \"T1\",T \"T2\"], teamsQ4 = T [T \"T1\",T \"T2\"]}]}" :: Text)
