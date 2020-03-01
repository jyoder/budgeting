module PriorityRecordSpec (spec) where

import Data.Csv (decodeByName)
import Data.Vector (fromList)
import qualified PriorityRecord
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeByName" $ do
    it "returns a PriorityRecord when all columns are present" $
      do
        decodeByName "Name,Priority Q1,Priority Q2,Priority Q3,Priority Q4\n49ers,Football,Soccer,Hockey,Foosball"
        `shouldBe` Right
          ( fromList ["Name", "Priority Q1", "Priority Q2", "Priority Q3", "Priority Q4"],
            fromList [PriorityRecord.T "49ers" "Football" "Soccer" "Hockey" "Foosball"]
          )
