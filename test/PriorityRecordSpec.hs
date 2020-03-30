module PriorityRecordSpec (spec) where

import qualified CommonSpecs
import qualified Data.Csv
import qualified Data.Vector
import qualified PriorityRecord
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to the fields in the record" $ do
      let record = PriorityRecord.T 1 "49ers" "Football" "Soccer" "Hockey" "Foosball"
      PriorityRecord.lineNumber record `shouldBe` 1
      PriorityRecord.team record `shouldBe` "49ers"
      PriorityRecord.priorityQ1 record `shouldBe` "Football"
      PriorityRecord.priorityQ2 record `shouldBe` "Soccer"
      PriorityRecord.priorityQ3 record `shouldBe` "Hockey"
      PriorityRecord.priorityQ4 record `shouldBe` "Foosball"
  describe "decodeByName" $ do
    it "returns a PriorityRecord when all columns are present" $
      do
        Data.Csv.decodeByName "Name,Priority Q1,Priority Q2,Priority Q3,Priority Q4\n49ers,Football,Soccer,Hockey,Foosball"
        `shouldBe` Right
          ( Data.Vector.fromList ["Name", "Priority Q1", "Priority Q2", "Priority Q3", "Priority Q4"],
            Data.Vector.fromList [PriorityRecord.T 0 "49ers" "Football" "Soccer" "Hockey" "Foosball"]
          )
  CommonSpecs.eqSpec
    (PriorityRecord.T 0 "A" "B" "C" "D" "E")
    (PriorityRecord.T 1 "A" "B" "C" "D" "E")
  CommonSpecs.ordSpec
    (PriorityRecord.T 0 "A" "B" "C" "D" "E")
    (PriorityRecord.T 1 "A" "B" "C" "D" "E")
