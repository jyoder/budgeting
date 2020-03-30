module TeammateRecordSpec (spec) where

import qualified CommonSpecs
import qualified Data.Csv
import qualified Data.Vector
import Protolude
import qualified TeammateRecord
import qualified Teams
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to fields in the record" $ do
      let (rams, raiders, dolphins) = (Teams.make ["Rams"], Teams.make ["Raiders"], Teams.make ["Dolphins"])
      let record = TeammateRecord.T 1 "123" "Bob Bobberson" "Sports" rams raiders rams dolphins
      TeammateRecord.lineNumber record `shouldBe` 1
      TeammateRecord.bhc record `shouldBe` "123"
      TeammateRecord.name record `shouldBe` "Bob Bobberson"
      TeammateRecord.department record `shouldBe` "Sports"
      TeammateRecord.teamsQ1 record `shouldBe` rams
      TeammateRecord.teamsQ2 record `shouldBe` raiders
      TeammateRecord.teamsQ3 record `shouldBe` rams
      TeammateRecord.teamsQ4 record `shouldBe` dolphins
  describe "decodeByName" $ do
    it "returns a TeammateRecord when all columns are present" $ do
      let (rams, raiders, dolphins) = (Teams.make ["Rams"], Teams.make ["Raiders"], Teams.make ["Dolphins"])
       in Data.Csv.decodeByName "Bhc,Name,Department,Teams Q1,Teams Q2,Teams Q3,Teams Q4\n123,Bob Bobberson,Sports,Rams,Raiders,Rams,Dolphins"
            `shouldBe` Right
              ( Data.Vector.fromList ["Bhc", "Name", "Department", "Teams Q1", "Teams Q2", "Teams Q3", "Teams Q4"],
                Data.Vector.fromList [TeammateRecord.T 0 "123" "Bob Bobberson" "Sports" rams raiders rams dolphins]
              )
  CommonSpecs.eqSpec
    (TeammateRecord.T 0 "A" "B" "C" teams teams teams teams)
    (TeammateRecord.T 1 "A" "B" "C" teams teams teams teams)
  CommonSpecs.ordSpec
    (TeammateRecord.T 0 "A" "B" "C" teams teams teams teams)
    (TeammateRecord.T 1 "A" "B" "C" teams teams teams teams)

teams :: Teams.T
teams = Teams.make ["Team"]
