module SalaryRecordSpec (spec) where

import qualified CommonSpecs
import qualified Data.Csv
import qualified Data.Vector
import Protolude
import qualified SalaryRecord
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to fields in the record" $ do
      let record = SalaryRecord.T 1 "123" "Bob Bobberson" 100000.12 110000.00 110001.10 110000.00
      SalaryRecord.lineNumber record `shouldBe` 1
      SalaryRecord.bhc record `shouldBe` "123"
      SalaryRecord.name record `shouldBe` "Bob Bobberson"
      SalaryRecord.salaryQ1 record `shouldBe` 100000.12
      SalaryRecord.salaryQ2 record `shouldBe` 110000.00
      SalaryRecord.salaryQ3 record `shouldBe` 110001.10
      SalaryRecord.salaryQ4 record `shouldBe` 110000.00
  describe "decodeByName" $ do
    it "returns a SalaryRecord when all columns are present" $ do
      Data.Csv.decodeByName "Bhc,Name,Salary Q1,Salary Q2,Salary Q3,Salary Q4\n123,Bob Bobberson,100000.12,110000.00,110001.10,110000"
        `shouldBe` Right
          ( Data.Vector.fromList ["Bhc", "Name", "Salary Q1", "Salary Q2", "Salary Q3", "Salary Q4"],
            Data.Vector.fromList [SalaryRecord.T 0 "123" "Bob Bobberson" 100000.12 110000.00 110001.10 110000.00]
          )
  CommonSpecs.eqSpec
    (SalaryRecord.T 0 "A" "B" 1 2 3 4)
    (SalaryRecord.T 1 "A" "B" 1 2 3 4)
  CommonSpecs.ordSpec
    (SalaryRecord.T 0 "A" "B" 1 2 3 4)
    (SalaryRecord.T 1 "A" "B" 1 2 3 4)
