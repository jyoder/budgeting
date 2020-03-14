module TeammateRecordSpec (spec) where

import Data.Csv (decodeByName)
import Data.Vector (fromList)
import Protolude
import qualified TeammateRecord
import qualified Teams
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to fields in the record" $ do
      let (rams, raiders, dolphins) = (Teams.make ["Rams"], Teams.make ["Raiders"], Teams.make ["Dolphins"])
      let record = TeammateRecord.T "123" "Bob Bobberson" rams raiders dolphins rams
      TeammateRecord.bhc record `shouldBe` "123"
      TeammateRecord.name record `shouldBe` "Bob Bobberson"
      TeammateRecord.teamsQ1 record `shouldBe` rams
      TeammateRecord.teamsQ2 record `shouldBe` raiders
      TeammateRecord.teamsQ3 record `shouldBe` dolphins
      TeammateRecord.teamsQ4 record `shouldBe` rams
  describe "decodeByName" $ do
    it "returns a TeammateRecord when all columns are present" $ do
      let (rams, raiders, dolphins) = (Teams.make ["Rams"], Teams.make ["Raiders"], Teams.make ["Dolphins"])
      decodeByName "Bhc,Name,Teams Q1,Teams Q2,Teams Q3,Teams Q4\n123,Bob Bobberson,Rams,Raiders,Dolphins,Rams"
        `shouldBe` Right
          ( fromList ["Bhc", "Name", "Teams Q1", "Teams Q2", "Teams Q3", "Teams Q4"],
            fromList [TeammateRecord.T "123" "Bob Bobberson" rams raiders dolphins rams]
          )
