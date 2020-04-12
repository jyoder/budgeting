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
    it "returns a ratio report with a 2:1 ratio when there are two SEs and one of every other role" $ do
      let hex = Teams.make ["Hex Pistols"]
          se1 = TeammateRecord.T 1 "10" "Bob" "Development" "Software Engineer" hex hex hex hex
          se2 = TeammateRecord.T 2 "11" "Rob" "Development" "Software Engineer" hex hex hex hex
          pm = TeammateRecord.T 3 "12" "Lob" "Development" "Product Manager" hex hex hex hex
          ux = TeammateRecord.T 4 "13" "Gob" "Development" "User Experience Designer" hex hex hex hex
          qa = TeammateRecord.T 5 "14" "Nob" "Development" "Quality Assurance Engineer" hex hex hex hex
       in RatioReportGenerator.generate [qa, se1, pm, se2, ux]
            `shouldBe` RatioReport.T
              [ RatioReport.Row "Product Manager" 2.0 2.0 2.0 2.0,
                RatioReport.Row "User Experience Designer" 2.0 2.0 2.0 2.0,
                RatioReport.Row "Quality Assurance Engineer" 2.0 2.0 2.0 2.0
              ]
    it "excludes teammates who are on the team \"None\" from the ratio calculation" $ do
      let hex = Teams.make ["Hex Pistols"]
          none = Teams.make ["None"]
          se1 = TeammateRecord.T 1 "10" "Bob" "Development" "Software Engineer" none hex none hex
          se2 = TeammateRecord.T 2 "11" "Rob" "Development" "Software Engineer" hex hex hex hex
          pm = TeammateRecord.T 3 "12" "Lob" "Development" "Product Manager" hex hex hex hex
          ux = TeammateRecord.T 4 "13" "Gob" "Development" "User Experience Designer" hex hex hex hex
          qa = TeammateRecord.T 5 "14" "Nob" "Development" "Quality Assurance Engineer" hex hex hex hex
       in RatioReportGenerator.generate [se1, se2, pm, ux, qa]
            `shouldBe` RatioReport.T
              [ RatioReport.Row "Product Manager" 1.0 2.0 1.0 2.0,
                RatioReport.Row "User Experience Designer" 1.0 2.0 1.0 2.0,
                RatioReport.Row "Quality Assurance Engineer" 1.0 2.0 1.0 2.0
              ]
    it "returns ratios of -1.0 when the denominator in the ratio is zero" $ do
      let hex = Teams.make ["Hex Pistols"]
          none = Teams.make ["None"]
          se = TeammateRecord.T 1 "10" "Bob" "Development" "Software Engineer" none hex none hex
       in RatioReportGenerator.generate [se]
            `shouldBe` RatioReport.T
              [ RatioReport.Row "Product Manager" (-1.0) (-1.0) (-1.0) (-1.0),
                RatioReport.Row "User Experience Designer" (-1.0) (-1.0) (-1.0) (-1.0),
                RatioReport.Row "Quality Assurance Engineer" (-1.0) (-1.0) (-1.0) (-1.0)
              ]
