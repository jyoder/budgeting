module InitiativeRecordSpec (spec) where

import Data.Csv (decodeByName)
import Data.Vector (fromList)
import InitiativeRecord (InitiativeRecord (..))
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeByName" $ do
    it "returns a InitiativeRecord when all columns are present" $ do
      decodeByName "Team,Initiative\n49ers,Football"
        `shouldBe` Right (fromList ["Team", "Initiative"], fromList [InitiativeRecord "49ers" "Football"])
