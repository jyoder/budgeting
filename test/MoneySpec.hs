module MoneySpec (spec) where

import qualified CommonSpecs
import Data.Csv (toField)
import qualified Money
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "Fractional" $ do
    it "makes the type behave like a fractional number" $ do
      (4.0 :: Money.T) / 2 `shouldBe` 2.0
      recip (4.0 :: Money.T) `shouldBe` 0.25
  describe "toField" $ do
    it "serializes the value to a CSV field in units of millions" $ do
      toField (1500000 :: Money.T) `shouldBe` "1.50"
  CommonSpecs.showSpec (1 :: Money.T) "T 1.0"
  CommonSpecs.eqSpec (1 :: Money.T) (2 :: Money.T)
  CommonSpecs.ordSpec (1 :: Money.T) (2 :: Money.T)
  CommonSpecs.numSpec (1 :: Money.T) (2 :: Money.T)
