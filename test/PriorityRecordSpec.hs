module PriorityRecordSpec (spec) where

import Data.Csv (decodeByName)
import Data.Vector (fromList)
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
        decodeByName "Name,Priority Q1,Priority Q2,Priority Q3,Priority Q4\n49ers,Football,Soccer,Hockey,Foosball"
        `shouldBe` Right
          ( fromList ["Name", "Priority Q1", "Priority Q2", "Priority Q3", "Priority Q4"],
            fromList [PriorityRecord.T 0 "49ers" "Football" "Soccer" "Hockey" "Foosball"]
          )
  describe "Eq" $ do
    it "tests whether two priority records are equal" $ do
      let record1 = PriorityRecord.T 0 "49ers" "Football" "Soccer" "Hockey" "Foosball"
      let record2 = PriorityRecord.T 1 "49ers" "Football" "Soccer" "Hockey" "Foosball"
      record1 `shouldNotBe` record2
  describe "Ord" $ do
    it "tests the ordering of priority records" $ do
      let record1 = PriorityRecord.T 0 "49ers" "Football" "Soccer" "Hockey" "Foosball"
      let record2 = PriorityRecord.T 0 "59ers" "Football" "Soccer" "Hockey" "Foosball"
      compare record1 record2 `shouldBe` LT
      record2 `shouldSatisfy` (<) record1
      record2 `shouldSatisfy` (<=) record1
      record1 `shouldSatisfy` (>) record2
      record1 `shouldSatisfy` (>=) record2
      max record1 record2 `shouldBe` record2
      min record1 record2 `shouldBe` record1
