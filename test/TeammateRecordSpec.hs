module TeammateRecordSpec (spec) where

import Data.Csv (decodeByName)
import Data.Vector (fromList)
import Protolude
import qualified TeammateRecord
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to fields in the record" $ do
      let record = TeammateRecord.T "123" "Bob Bobberson" "49ers" "Raiders" "Dolphins" "49ers"
      TeammateRecord.bhc record `shouldBe` "123"
      TeammateRecord.name record `shouldBe` "Bob Bobberson"
      TeammateRecord.teamsQ1 record `shouldBe` "49ers"
      TeammateRecord.teamsQ2 record `shouldBe` "Raiders"
      TeammateRecord.teamsQ3 record `shouldBe` "Dolphins"
      TeammateRecord.teamsQ4 record `shouldBe` "49ers"
  describe "decodeByName" $ do
    it "returns a TeammateRecord when all columns are present" $ do
      decodeByName "Bhc,Name,Teams Q1,Teams Q2,Teams Q3,Teams Q4\n123,Bob Bobberson,49ers,Raiders,Dolphins,49ers"
        `shouldBe` Right
          ( fromList ["Bhc", "Name", "Teams Q1", "Teams Q2", "Teams Q3", "Teams Q4"],
            fromList [TeammateRecord.T "123" "Bob Bobberson" "49ers" "Raiders" "Dolphins" "49ers"]
          )
