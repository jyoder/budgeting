module BudgetRecordsSpec (spec) where

import qualified BudgetRecords
import qualified PriorityRecord
import Protolude
import qualified SalaryRecord
import qualified TeammateRecord
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to the fields in the record" $ do
      let (priority, salary, teammate, record) =
            ( PriorityRecord.T "Team" "P1" "P2" "P3" "P4",
              SalaryRecord.T "Bhc" "Name" 1 2 3 4,
              TeammateRecord.T "Bhc" "Name" "T1" "T2" "T3" "T4",
              BudgetRecords.T [priority] [salary] [teammate]
            )
      BudgetRecords.priorityRecords record `shouldBe` [priority]
      BudgetRecords.salaryRecords record `shouldBe` [salary]
      BudgetRecords.teammateRecords record `shouldBe` [teammate]
