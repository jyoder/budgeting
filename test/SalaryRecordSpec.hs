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
      decodeByName "Budget hc#,\"Last name, First name\",Total Annualized Comp @ Target\nBob,Bobberson,100000.12"
        `shouldBe` Right
          ( fromList ["Budget hc#", "Last name, First name", "Total Annualized Comp @ Target"],
            fromList [SalaryRecord "Bob" "Bobberson" 100000.12]
          )
