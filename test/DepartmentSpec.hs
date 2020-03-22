module DepartmentSpec (spec) where

import qualified Department
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "excludedFromDev" $ do
    it "returns true if the department is \"Web Operations\"" $ do
      "Web Operations" `shouldSatisfy` Department.excludedFromDev
    it "returns true if the department is \"Security\"" $ do
      "Security" `shouldSatisfy` Department.excludedFromDev
    it "returns false if the department is not \"Security\" or \"Web Operations\"" $ do
      "Software Engineering" `shouldSatisfy` not . Department.excludedFromDev
  describe "toText" $ do
    it "converts a department to text" $ do
      Department.toText "science" `shouldBe` "science"
  describe "show" $ do
    it "converts a department to text" $ do
      show ("science" :: Department.T) `shouldBe` ("T \"science\"" :: Text)
  describe "Eq" $ do
    it "returns whether two departments are equal" $ do
      ("science" :: Department.T) `shouldNotBe` ("english" :: Department.T)
  describe "Ord" $ do
    it "tests the ordering of departments" $ do
      let (happy, nappy) = ("happy" :: Department.T, "nappy" :: Department.T)
      compare happy nappy `shouldBe` LT
      nappy `shouldSatisfy` (<) happy
      nappy `shouldSatisfy` (<=) happy
      happy `shouldSatisfy` (>) nappy
      happy `shouldSatisfy` (>=) nappy
      max happy nappy `shouldBe` nappy
      min happy nappy `shouldBe` happy
