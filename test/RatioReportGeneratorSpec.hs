module RatioReportGeneratorSpec (spec) where

import Protolude
import qualified RatioReport
import qualified RatioReportGenerator
import qualified TeammateRecord
import qualified Teams
import Test.Hspec

spec :: Spec
spec = do
  describe "generate" $ do
    it "returns a ratio report with a 2:1 ratio when there are two SEs and one QA" $ do
      let hex = Teams.make ["Hex Pistols"]
          se1 = TeammateRecord.T 1 "10" "Bob" "Development" "Software Engineer" hex hex hex hex
          qa = TeammateRecord.T 2 "12" "Gob" "Development" "Quality Assurance Engineer" hex hex hex hex
          se2 = TeammateRecord.T 3 "11" "Rob" "Development" "Software Engineer" hex hex hex hex
       in RatioReportGenerator.generate [se1, qa, se2]
            `shouldBe` RatioReport.T [RatioReport.Row "Quality Assurance Engineer" 2.0 2.0 2.0 2.0]
    it "excludes teammates who are on the team \"None\" from the ratio calculation" $ do
      let hex = Teams.make ["Hex Pistols"]
          none = Teams.make ["None"]
          se1 = TeammateRecord.T 1 "10" "Bob" "Development" "Software Engineer" none hex none hex
          qa = TeammateRecord.T 2 "12" "Gob" "Development" "Quality Assurance Engineer" hex hex hex hex
          se2 = TeammateRecord.T 3 "11" "Rob" "Development" "Software Engineer" hex hex hex hex
       in RatioReportGenerator.generate [se1, qa, se2]
            `shouldBe` RatioReport.T [RatioReport.Row "Quality Assurance Engineer" 1.0 2.0 1.0 2.0]
