module PreprocessorSpec (spec) where

import qualified BudgetRecords
import qualified Preprocessor
import qualified PriorityRecord
import Protolude
import qualified SalaryRecord
import qualified TeammateRecord
import qualified Teams
import Test.Hspec

spec :: Spec
spec = do
  describe "preprocess" $ do
    it "adds line numbers to priorities" $ do
      let priority1 = PriorityRecord.T 0 "Rams" "P1" "P2" "P3" "P4"
      let priority2 = PriorityRecord.T (-1) "Goats" "P1" "P2" "P3" "P4"
      let records = BudgetRecords.T [priority1, priority2] [] []
      Preprocessor.preprocess records
        `shouldBe` BudgetRecords.T
          [ PriorityRecord.T 1 "Rams" "P1" "P2" "P3" "P4",
            PriorityRecord.T 2 "Goats" "P1" "P2" "P3" "P4"
          ]
          []
          []
    it "adds line numbers to salaries" $ do
      let salary1 = SalaryRecord.T 0 "Bob" "123" 100.0 100.0 100.0 100.0
      let salary2 = SalaryRecord.T (-1) "Rob" "124" 100.0 100.0 100.0 100.0
      let records = BudgetRecords.T [] [salary1, salary2] []
      Preprocessor.preprocess records
        `shouldBe` BudgetRecords.T
          []
          [ SalaryRecord.T 1 "Bob" "123" 100.0 100.0 100.0 100.0,
            SalaryRecord.T 2 "Rob" "124" 100.0 100.0 100.0 100.0
          ]
          []
    it "adds line numbers to teammates" $ do
      let rams = Teams.make ["Rams"]
      let teammate1 = TeammateRecord.T 0 "Bob" "123" "Sports" rams rams rams rams
      let teammate2 = TeammateRecord.T (-1) "Rob" "124" "Sports" rams rams rams rams
      let records = BudgetRecords.T [] [] [teammate1, teammate2]
      Preprocessor.preprocess records
        `shouldBe` BudgetRecords.T
          []
          []
          [ TeammateRecord.T 1 "Bob" "123" "Sports" rams rams rams rams,
            TeammateRecord.T 2 "Rob" "124" "Sports" rams rams rams rams
          ]
