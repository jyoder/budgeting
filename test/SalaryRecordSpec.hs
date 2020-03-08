module SalaryRecordSpec (spec) where

import Data.Csv (decodeByName)
import Data.Vector (fromList)
import Protolude
import qualified SalaryRecord
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to fields in the record" $ do
      let record = SalaryRecord.T "123" "Bob Bobberson" 100000.12 110000.00 110001.10 110000.00
      SalaryRecord.bhc record `shouldBe` "123"
      SalaryRecord.name record `shouldBe` "Bob Bobberson"
      SalaryRecord.salaryQ1 record `shouldBe` 100000.12
      SalaryRecord.salaryQ2 record `shouldBe` 110000.00
      SalaryRecord.salaryQ3 record `shouldBe` 110001.10
      SalaryRecord.salaryQ4 record `shouldBe` 110000.00
  describe "decodeByName" $ do
    it "returns a SalaryRecord when all columns are present" $ do
      decodeByName "Bhc,Name,Salary Q1,Salary Q2,Salary Q3,Salary Q4\n123,Bob Bobberson,100000.12,110000.00,110001.10,110000"
        `shouldBe` Right
          ( fromList ["Bhc", "Name", "Salary Q1", "Salary Q2", "Salary Q3", "Salary Q4"],
            fromList [SalaryRecord.T "123" "Bob Bobberson" 100000.12 110000.00 110001.10 110000.00]
          )
