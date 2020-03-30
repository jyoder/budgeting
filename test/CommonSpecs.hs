module CommonSpecs
  ( showSpec,
    eqSpec,
    ordSpec,
    numSpec,
    toTextSpec,
    fromTextSpec,
  )
where

import Protolude
import Test.Hspec

showSpec :: Show a => a -> Text -> Spec
showSpec value text = do
  describe "Show" $ do
    it "returns a text representation of the value" $ do
      show value `shouldBe` text

eqSpec :: (Eq a, Show a) => a -> a -> Spec
eqSpec value1 value2 = do
  describe "Eq" $ do
    it "returns whether to values are equal" $ do
      value1 `shouldNotBe` value2

ordSpec :: (Ord a, Show a) => a -> a -> Spec
ordSpec value1 value2 = do
  describe "Ord" $ do
    it "compares the order of two values" $ do
      compare value1 value2 `shouldBe` LT
      value2 `shouldSatisfy` (<) value1
      value2 `shouldSatisfy` (<=) value1
      value1 `shouldSatisfy` (>) value2
      value1 `shouldSatisfy` (>=) value2
      max value1 value2 `shouldBe` value2
      min value1 value2 `shouldBe` value1

numSpec :: (Num a, Eq a, Show a) => a -> a -> Spec
numSpec num1 num2 = do
  describe "Num" $ do
    it "makes the type behave like a number" $ do
      num1 + num2 `shouldBe` 3
      num2 - num1 `shouldBe` 1
      num1 * num2 `shouldBe` 2
      abs num1 `shouldBe` 1
      signum num1 `shouldBe` 1
      negate num1 `shouldBe` -1

toTextSpec :: (a -> Text) -> a -> Text -> Spec
toTextSpec toText value text = do
  describe "toText" $ do
    it "returns a text representation of the value" $ do
      toText value `shouldBe` text

fromTextSpec :: (Eq a, Show a) => (Text -> a) -> Text -> a -> Spec
fromTextSpec fromText text value = do
  describe "fromText" $ do
    it "returns a value corresponding to some text" $ do
      fromText text `shouldBe` value
