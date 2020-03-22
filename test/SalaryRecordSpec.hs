module SalaryRecordSpec (spec) where

import qualified Data.Csv
import qualified Data.Vector
import Protolude
import qualified Salary
import qualified SalaryRecord
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to fields in the record" $ do
      let (sQ1, sQ2, sQ3, sQ4) = (Salary.make 100000.12, Salary.make 110000.00, Salary.make 110001.10, Salary.make 110000.00)
      let record = SalaryRecord.T 1 "123" "Bob Bobberson" sQ1 sQ2 sQ3 sQ4
      SalaryRecord.bhc record `shouldBe` "123"
      SalaryRecord.name record `shouldBe` "Bob Bobberson"
      SalaryRecord.salaryQ1 record `shouldBe` sQ1
      SalaryRecord.salaryQ2 record `shouldBe` sQ2
      SalaryRecord.salaryQ3 record `shouldBe` sQ3
      SalaryRecord.salaryQ4 record `shouldBe` sQ4
  describe "decodeByName" $ do
    it "returns a SalaryRecord when all columns are present" $ do
      let (sQ1, sQ2, sQ3, sQ4) = (Salary.make 100000.12, Salary.make 110000.00, Salary.make 110001.10, Salary.make 110000.00)
      Data.Csv.decodeByName "Bhc,Name,Salary Q1,Salary Q2,Salary Q3,Salary Q4\n123,Bob Bobberson,100000.12,110000.00,110001.10,110000"
        `shouldBe` Right
          ( Data.Vector.fromList ["Bhc", "Name", "Salary Q1", "Salary Q2", "Salary Q3", "Salary Q4"],
            Data.Vector.fromList [SalaryRecord.T 0 "123" "Bob Bobberson" sQ1 sQ2 sQ3 sQ4]
          )
