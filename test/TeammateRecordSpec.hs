module TeammateRecordSpec (spec) where

import qualified CommonSpecs
import qualified Data.Csv
import qualified Data.Vector
import Protolude
import qualified Quarter
import qualified TeammateRecord
import qualified Teams
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to fields in the record" $ do
      let (rams, raiders, dolphins) = (Teams.make ["Rams"], Teams.make ["Raiders"], Teams.make ["Dolphins"])
      let record = TeammateRecord.T 1 "123" "Bob Bobberson" "Sports" "Player" rams raiders rams dolphins
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
       in Data.Csv.decodeByName "Bhc,Name,Department,Function,Teams Q1,Teams Q2,Teams Q3,Teams Q4\n123,Bob Bobberson,Sports,Player,Rams,Raiders,Rams,Dolphins"
            `shouldBe` Right
              ( Data.Vector.fromList ["Bhc", "Name", "Department", "Function", "Teams Q1", "Teams Q2", "Teams Q3", "Teams Q4"],
                Data.Vector.fromList [TeammateRecord.T 0 "123" "Bob Bobberson" "Sports" "Player" rams raiders rams dolphins]
              )
  describe "teams" $ do
    it "returns the teams for the specified quarter" $ do
      let rams = Teams.make ["Rams"]
      let raiders = Teams.make ["Raiders"]
      let dolphins = Teams.make ["Dolphins"]
      let chefs = Teams.make ["Chefs"]
      let record = TeammateRecord.T 1 "123" "Bob" "Sports" "Player" rams raiders dolphins chefs
      TeammateRecord.teams Quarter.Q1 record `shouldBe` rams
      TeammateRecord.teams Quarter.Q2 record `shouldBe` raiders
      TeammateRecord.teams Quarter.Q3 record `shouldBe` dolphins
      TeammateRecord.teams Quarter.Q4 record `shouldBe` chefs
  describe "withDefaultTeam" $ do
    it "returns a teammate record with a default team of \"None\" for any quarter with no teams" $ do
      let blank = Teams.make []
          none = Teams.make ["None"]
          record = TeammateRecord.T 1 "123" "Bob" "Sports" "Player" blank blank blank blank
       in TeammateRecord.withDefaultTeam record
            `shouldBe` TeammateRecord.T 1 "123" "Bob" "Sports" "Player" none none none none
    it "leaves the teams for a quarter alone if they are not empty" $ do
      let rams = Teams.make ["Rams"]
          record = TeammateRecord.T 1 "123" "Bob" "Sports" "Player" rams rams rams rams
       in TeammateRecord.withDefaultTeam record
            `shouldBe` TeammateRecord.T 1 "123" "Bob" "Sports" "Player" rams rams rams rams
  CommonSpecs.eqSpec
    (TeammateRecord.T 0 "A" "B" "C" "Player" teams teams teams teams)
    (TeammateRecord.T 1 "A" "B" "C" "Player" teams teams teams teams)
  CommonSpecs.ordSpec
    (TeammateRecord.T 0 "A" "B" "C" "Player" teams teams teams teams)
    (TeammateRecord.T 1 "A" "B" "C" "Player" teams teams teams teams)

teams :: Teams.T
teams = Teams.make ["Team"]
