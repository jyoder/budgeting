module ResultSpec (spec) where

import qualified Error
import Protolude
import qualified Result
import Test.Hspec

spec :: Spec
spec = do
  describe "success" $ do
    it "returns a success result" $ do
      Result.success (1 :: Integer) `shouldBe` Right 1
  describe "error" $ do
    it "returns an error result" $ do
      Result.error "Blam!" `shouldBe` (Left $ Error.make "Blam!" :: Result.T Integer)
  describe "prepend" $ do
    it "returns a new result with prepended error context" $ do
      Result.prepend "Prepended " (Error.make "error")
        `shouldBe` (Left $ Error.make "Prepended error" :: Result.T Integer)
