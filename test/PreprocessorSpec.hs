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
    it "adds a default team to any teammate who is not on a team" $ do
      let blank = Teams.make []
          none = Teams.make ["None"]
          rams = Teams.make ["Rams"]
          teammate1 = TeammateRecord.T 2 "Bob" "123" "Sports" blank blank blank blank
          teammate2 = TeammateRecord.T 3 "Rob" "124" "Sports" rams rams rams rams
          records = BudgetRecords.T [] [] [teammate1, teammate2]
       in Preprocessor.preprocess records
            `shouldBe` BudgetRecords.T
              []
              []
              [ TeammateRecord.T 2 "Bob" "123" "Sports" none none none none,
                TeammateRecord.T 3 "Rob" "124" "Sports" rams rams rams rams
              ]
    it "adds line numbers to priorities" $ do
      let priority1 = PriorityRecord.T 0 "Rams" "P1" "P2" "P3" "P4"
          priority2 = PriorityRecord.T (-1) "Goats" "P1" "P2" "P3" "P4"
          records = BudgetRecords.T [priority1, priority2] [] []
       in Preprocessor.preprocess records
            `shouldBe` BudgetRecords.T
              [ PriorityRecord.T 2 "Rams" "P1" "P2" "P3" "P4",
                PriorityRecord.T 3 "Goats" "P1" "P2" "P3" "P4"
              ]
              []
              []
    it "adds line numbers to salaries" $ do
      let salary1 = SalaryRecord.T 0 "Bob" "123" 100.0 100.0 100.0 100.0
          salary2 = SalaryRecord.T (-1) "Rob" "124" 100.0 100.0 100.0 100.0
          records = BudgetRecords.T [] [salary1, salary2] []
       in Preprocessor.preprocess records
            `shouldBe` BudgetRecords.T
              []
              [ SalaryRecord.T 2 "Bob" "123" 100.0 100.0 100.0 100.0,
                SalaryRecord.T 3 "Rob" "124" 100.0 100.0 100.0 100.0
              ]
              []
    it "adds line numbers to teammates" $ do
      let rams = Teams.make ["Rams"]
          teammate1 = TeammateRecord.T 0 "Bob" "123" "Sports" rams rams rams rams
          teammate2 = TeammateRecord.T (-1) "Rob" "124" "Sports" rams rams rams rams
          records = BudgetRecords.T [] [] [teammate1, teammate2]
       in Preprocessor.preprocess records
            `shouldBe` BudgetRecords.T
              []
              []
              [ TeammateRecord.T 2 "Bob" "123" "Sports" rams rams rams rams,
                TeammateRecord.T 3 "Rob" "124" "Sports" rams rams rams rams
              ]
    it "removes teammates who are in the \"Web Operations\" department" $ do
      let rams = Teams.make ["Rams"]
          teammate1 = TeammateRecord.T 0 "Bob" "123" "Web Operations" rams rams rams rams
          teammate2 = TeammateRecord.T (-1) "Rob" "124" "Sports" rams rams rams rams
          records = BudgetRecords.T [] [] [teammate1, teammate2]
       in Preprocessor.preprocess records
            `shouldBe` BudgetRecords.T
              []
              []
              [ TeammateRecord.T 3 "Rob" "124" "Sports" rams rams rams rams
              ]
    it "removes teammates who are in the \"Web Operations\" department" $ do
      let rams = Teams.make ["Rams"]
          teammate1 = TeammateRecord.T 0 "Bob" "123" "Security" rams rams rams rams
          teammate2 = TeammateRecord.T (-1) "Rob" "124" "Sports" rams rams rams rams
          records = BudgetRecords.T [] [] [teammate1, teammate2]
       in Preprocessor.preprocess records
            `shouldBe` BudgetRecords.T
              []
              []
              [ TeammateRecord.T 3 "Rob" "124" "Sports" rams rams rams rams
              ]
