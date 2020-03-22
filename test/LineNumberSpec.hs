module LineNumberSpec (spec) where

import qualified LineNumber
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "next" $ do
    it "returns the next successive line number" $ do
      LineNumber.next 123 `shouldBe` 124
  describe "toText" $ do
    it "converts a line number to text" $ do
      LineNumber.toText 123 `shouldBe` "123"
  describe "show" $ do
    it "converts a line number to text" $ do
      show (123 :: LineNumber.T) `shouldBe` ("T 123" :: Text)
  describe "Eq" $ do
    it "returns whether two line numbers are equal" $ do
      (1 :: LineNumber.T) `shouldNotBe` (2 :: LineNumber.T)
  describe "Ord" $ do
    it "tests the ordering of line number" $ do
      let (line1, line2) = (1 :: LineNumber.T, 2 :: LineNumber.T)
      compare line1 line2 `shouldBe` LT
      line2 `shouldSatisfy` (<) line1
      line2 `shouldSatisfy` (<=) line1
      line1 `shouldSatisfy` (>) line2
      line1 `shouldSatisfy` (>=) line2
      max line1 line2 `shouldBe` line2
      min line1 line2 `shouldBe` line1
  describe "Num" $ do
    it "makes a line number behave like a number" $ do
      let (line1, line2) = (1 :: LineNumber.T, 2 :: LineNumber.T)
      line1 + line2 `shouldBe` 3
      line2 - line1 `shouldBe` 1
      line1 * line2 `shouldBe` 2
      abs line1 `shouldBe` 1
      signum line1 `shouldBe` 1
      negate line1 `shouldBe` -1
