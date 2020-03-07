module ApplicationSpec (spec) where

import qualified Application
import qualified Argument
import qualified MockSystem
import Protolude
import qualified StateActions
import Test.Hspec

spec :: Spec
spec = do
  describe "run" $ do
    it "prints a helpful usage message when fewer than 3 command line arguments are given" $ do
      let system = MockSystem.make (Argument.fromText <$> ["arg1", "arg2"]) []
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["Usage: ./budgeting-exe <priorities-csv> <salaries-csv> <teammates-csv>"]
