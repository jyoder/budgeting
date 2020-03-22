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
