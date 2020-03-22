module PrioritySpec (spec) where

import qualified Priority
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "show" $ do
    it "converts a priority to text" $ do
      show ("Drums" :: Priority.T) `shouldBe` ("T \"Drums\"" :: Text)
  describe "Eq" $ do
    it "returns whether two priorities are equal" $ do
      ("Drums" :: Priority.T) `shouldNotBe` ("Trombone" :: Priority.T)
  describe "Ord" $ do
    it "tests the ordering of priorities" $ do
      let (happy, nappy) = ("happy" :: Priority.T, "nappy" :: Priority.T)
      compare happy nappy `shouldBe` LT
      nappy `shouldSatisfy` (<) happy
      nappy `shouldSatisfy` (<=) happy
      happy `shouldSatisfy` (>) nappy
      happy `shouldSatisfy` (>=) nappy
      max happy nappy `shouldBe` nappy
      min happy nappy `shouldBe` happy
