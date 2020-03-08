module TeammateRecordSpec (spec) where

import Data.Csv (decodeByName)
import Data.Vector (fromList)
import Protolude
import qualified TeammateRecord
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeByName" $ do
    it "returns a TeammateRecord when all columns are present" $ do
      decodeByName "Bhc,Name,Teams Q1,Teams Q2,Teams Q3,Teams Q4\nBob Bobberson,123,49ers,Raiders,Dolphins,49ers"
        `shouldBe` Right
          ( fromList ["Bhc", "Name", "Teams Q1", "Teams Q2", "Teams Q3", "Teams Q4"],
            fromList [TeammateRecord.T "Bob Bobberson" "123" "49ers" "Raiders" "Dolphins" "49ers"]
          )
