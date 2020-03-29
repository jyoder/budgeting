module CostCalculatorSpec (spec) where

import qualified CostCalculator
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "cost" $ do
    it "returns the true cost of a person's salary" $ do
      CostCalculator.cost 100000 `shouldBe` 200000.00
