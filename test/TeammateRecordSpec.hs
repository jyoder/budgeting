module TeammateRecordSpec (spec) where

import Data.Csv (decodeByName)
import Data.Vector (fromList)
import Protolude
import TeammateRecord (TeammateRecord (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeByName" $ do
    it "returns a TeammateRecord when all columns are present" $ do
      decodeByName "Budget hc#,First Name,Last Name,Team\n123,Bob,Bobberson,49ers"
        `shouldBe` Right (fromList ["Budget hc#", "First Name", "Last Name", "Team"], fromList [TeammateRecord "123" "Bob" "Bobberson" "49ers"])
