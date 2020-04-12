module RatioReportSpec (spec) where

import qualified CommonSpecs
import Protolude
import qualified RatioReport
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provide access to fields in the record" $ do
      let row = RatioReport.Row "QA" 1.0 2.0 3.0 4.0
      RatioReport.role row `shouldBe` "QA"
      RatioReport.devsPerRoleQ1 row `shouldBe` 1.0
      RatioReport.devsPerRoleQ2 row `shouldBe` 2.0
      RatioReport.devsPerRoleQ3 row `shouldBe` 3.0
      RatioReport.devsPerRoleQ4 row `shouldBe` 4.0
  describe "toCsv" $ do
    it "returns a ratio report in CSV format" $ do
      let report = RatioReport.T [RatioReport.Row "UX" 1.0 2.0 3.0 4.0]
       in RatioReport.toCsv report `shouldBe` "Role,Devs Per Role Q1,Devs Per Role Q2,Devs Per Role Q3,Devs Per Role Q4\r\nUX,1.0,2.0,3.0,4.0\r\n"
  CommonSpecs.showSpec
    (RatioReport.T [RatioReport.Row "UX" 1.0 1.0 1.0 1.0])
    "T [Row {role = T \"UX\", devsPerRoleQ1 = 1.0, devsPerRoleQ2 = 1.0, devsPerRoleQ3 = 1.0, devsPerRoleQ4 = 1.0}]"
  CommonSpecs.eqSpec
    (RatioReport.T [RatioReport.Row "QA" 1.0 1.0 1.0 1.0])
    (RatioReport.T [RatioReport.Row "PM" 3.0 3.0 3.0 3.0])
