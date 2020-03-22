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
      decodeByName "Bhc,Name,Department,Teams Q1,Teams Q2,Teams Q3,Teams Q4\n123,Bob Bobberson,Sports,Rams,Raiders,Rams,Dolphins"
        `shouldBe` Right
          ( fromList ["Bhc", "Name", "Department", "Teams Q1", "Teams Q2", "Teams Q3", "Teams Q4"],
            fromList [TeammateRecord.T 0 "123" "Bob Bobberson" "Sports" rams raiders rams dolphins]
          )
  describe "Eq" $ do
    it "tests whether two teammate records are equal" $ do
      let (rams, raiders, dolphins) = (Teams.make ["Rams"], Teams.make ["Raiders"], Teams.make ["Dolphins"])
      let record1 = TeammateRecord.T 1 "123" "Bob Bobberson" "Sports" rams raiders rams dolphins
      let record2 = TeammateRecord.T 2 "124" "Bob Bobbers" "Sports" rams raiders rams dolphins
      record1 `shouldNotBe` record2
  describe "Ord" $ do
    it "tests the ordering of teammate records" $ do
      let (rams, raiders, bears) = (Teams.make ["Rams"], Teams.make ["Raiders"], Teams.make ["Bears"])
      let record1 = TeammateRecord.T 1 "123" "Bob Bobberson" "Sports" rams raiders rams bears
      let record2 = TeammateRecord.T 2 "124" "Bob Bobbers" "Sports" rams raiders rams bears
      compare record1 record2 `shouldBe` LT
      record2 `shouldSatisfy` (<) record1
      record2 `shouldSatisfy` (<=) record1
      record1 `shouldSatisfy` (>) record2
      record1 `shouldSatisfy` (>=) record2
      max record1 record2 `shouldBe` record2
      min record1 record2 `shouldBe` record1
