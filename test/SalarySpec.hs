module SalarySpec (spec) where

import Protolude
import qualified Salary
import Test.Hspec

spec :: Spec
spec = do
  describe "show" $ do
    it "converts a salary to text" $ do
      show (123.0 :: Salary.T) `shouldBe` ("T 123.0" :: Text)
  describe "Eq" $ do
    it "returns whether two salaries are equal" $ do
      (123.0 :: Salary.T) `shouldNotBe` (123.1 :: Salary.T)
  describe "Ord" $ do
    it "tests the ordering of salaries" $ do
      let (salary1, salary2) = (123.0 :: Salary.T, 124.0 :: Salary.T)
      compare salary1 salary2 `shouldBe` LT
      salary2 `shouldSatisfy` (<) salary1
      salary2 `shouldSatisfy` (<=) salary1
      salary1 `shouldSatisfy` (>) salary2
      salary1 `shouldSatisfy` (>=) salary2
      max salary1 salary2 `shouldBe` salary2
      min salary1 salary2 `shouldBe` salary1
  describe "Num" $ do
    it "makes a line number behave like a number" $ do
      let (line1, line2) = (1 :: Salary.T, 2 :: Salary.T)
      line1 + line2 `shouldBe` 3
      line2 - line1 `shouldBe` 1
      line1 * line2 `shouldBe` 2
      abs line1 `shouldBe` 1
      signum line1 `shouldBe` 1
      negate line1 `shouldBe` -1
  describe "Fractional" $ do
    it "behaves like a fractional number" $ do
      (4.0 :: Salary.T) / 2 `shouldBe` 2.0
      recip (4.0 :: Salary.T) `shouldBe` 0.25
