module SalaryRecordSpec (spec) where

import Data.Csv (decodeByName)
import Data.Vector (fromList)
import Protolude
import SalaryRecord (SalaryRecord (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeByName" $ do
    it "returns a SalaryRecord when all columns are present" $ do
      decodeByName "Bhc,Name,Salary Q1,Salary Q2,Salary Q3,Salary Q4\n123,Bob Bobberson,100000.12,110000.00,110001.10,110000"
        `shouldBe` Right
          ( fromList ["Bhc", "Name", "Salary Q1", "Salary Q2", "Salary Q3", "Salary Q4"],
            fromList [SalaryRecord "123" "Bob Bobberson" 100000.12 110000.00 110001.10 110000.00]
          )
