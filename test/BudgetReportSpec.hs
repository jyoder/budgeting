module BudgetReportSpec (spec) where

import qualified BudgetReport
import qualified CommonSpecs
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to the fields in the record" $ do
      let row = BudgetReport.Row "P" 1.00 2.00 3.00 4.00
      BudgetReport.priority row `shouldBe` "P"
      BudgetReport.spendQ1 row `shouldBe` 1.00
      BudgetReport.spendQ2 row `shouldBe` 2.00
      BudgetReport.spendQ3 row `shouldBe` 3.00
      BudgetReport.spendQ4 row `shouldBe` 4.00
  describe "toCsv" $ do
    it "returns a budget reports in CSV format" $ do
      let budgetReport = BudgetReport.T [BudgetReport.Row "P" 1000000.00 2000000.00 0.00 0.00]
       in BudgetReport.toCsv budgetReport
            `shouldBe` "Priority,Spend Q1,Spend Q2,Spend Q3,Spend Q4,Spend FY\r\nP,1.00,2.00,0.00,0.00,3.00\r\n"
  CommonSpecs.showSpec
    (BudgetReport.T [BudgetReport.Row "P" 1.00 2.00 3.00 4.00])
    "T [Row {priority = T \"P\", spendQ1 = T 1.0, spendQ2 = T 2.0, spendQ3 = T 3.0, spendQ4 = T 4.0}]"
  CommonSpecs.eqSpec
    (BudgetReport.T [BudgetReport.Row "P1" 1.00 2.00 3.00 4.00])
    (BudgetReport.T [BudgetReport.Row "P2" 1.00 2.00 3.00 4.00])
