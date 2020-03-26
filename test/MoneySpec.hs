module MoneySpec (spec) where

import qualified Money
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "show" $ do
    it "converts a money to text" $ do
      show (123.0 :: Money.T) `shouldBe` ("T 123.0" :: Text)
  describe "Eq" $ do
    it "returns whether two salaries are equal" $ do
      (123.0 :: Money.T) `shouldNotBe` (123.1 :: Money.T)
  describe "Ord" $ do
    it "tests the ordering of salaries" $ do
      let (money1, money2) = (123.0 :: Money.T, 124.0 :: Money.T)
      compare money1 money2 `shouldBe` LT
      money2 `shouldSatisfy` (<) money1
      money2 `shouldSatisfy` (<=) money1
      money1 `shouldSatisfy` (>) money2
      money1 `shouldSatisfy` (>=) money2
      max money1 money2 `shouldBe` money2
      min money1 money2 `shouldBe` money1
  describe "Num" $ do
    it "behaves like a number" $ do
      let (line1, line2) = (1 :: Money.T, 2 :: Money.T)
      line1 + line2 `shouldBe` 3
      line2 - line1 `shouldBe` 1
      line1 * line2 `shouldBe` 2
      abs line1 `shouldBe` 1
      signum line1 `shouldBe` 1
      negate line1 `shouldBe` -1
  describe "Fractional" $ do
    it "behaves like a fractional number" $ do
      (4.0 :: Money.T) / 2 `shouldBe` 2.0
      recip (4.0 :: Money.T) `shouldBe` 0.25
