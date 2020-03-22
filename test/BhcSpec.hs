module BhcSpec (spec) where

import qualified Bhc
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "toText" $ do
    it "converts a BHC to text" $ do
      Bhc.toText "123" `shouldBe` "123"
  describe "show" $ do
    it "converts a BHC to text" $ do
      show ("123" :: Bhc.T) `shouldBe` ("T \"123\"" :: Text)
  describe "Eq" $ do
    it "returns whether two BHCs are equal" $ do
      ("bhc1" :: Bhc.T) `shouldNotBe` ("bhc2" :: Bhc.T)
  describe "Ord" $ do
    it "tests the ordering of BHCs" $ do
      let (happy, nappy) = ("happy" :: Bhc.T, "nappy" :: Bhc.T)
      compare happy nappy `shouldBe` LT
      nappy `shouldSatisfy` (<) happy
      nappy `shouldSatisfy` (<=) happy
      happy `shouldSatisfy` (>) nappy
      happy `shouldSatisfy` (>=) nappy
      max happy nappy `shouldBe` nappy
      min happy nappy `shouldBe` happy
