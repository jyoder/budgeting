module NameSpec (spec) where

import qualified Name
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "show" $ do
    it "converts a name to text" $ do
      show ("Bob" :: Name.T) `shouldBe` ("T \"Bob\"" :: Text)
  describe "Eq" $ do
    it "returns whether two names are equal" $ do
      ("Bob" :: Name.T) `shouldNotBe` ("Bill" :: Name.T)
  describe "Ord" $ do
    it "tests the ordering of names" $ do
      let (happy, nappy) = ("happy" :: Name.T, "nappy" :: Name.T)
      compare happy nappy `shouldBe` LT
      nappy `shouldSatisfy` (<) happy
      nappy `shouldSatisfy` (<=) happy
      happy `shouldSatisfy` (>) nappy
      happy `shouldSatisfy` (>=) nappy
      max happy nappy `shouldBe` nappy
      min happy nappy `shouldBe` happy
