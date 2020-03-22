module TeamSpec (spec) where

import Protolude
import qualified Team
import Test.Hspec

spec :: Spec
spec = do
  describe "show" $ do
    it "converts a team to text" $ do
      show ("Bob" :: Team.T) `shouldBe` ("T \"Bob\"" :: Text)
  describe "Eq" $ do
    it "returns whether two teams are equal" $ do
      ("Bob" :: Team.T) `shouldNotBe` ("Bill" :: Team.T)
  describe "Ord" $ do
    it "tests the ordering of teams" $ do
      let (happy, nappy) = ("happy" :: Team.T, "nappy" :: Team.T)
      compare happy nappy `shouldBe` LT
      nappy `shouldSatisfy` (<) happy
      nappy `shouldSatisfy` (<=) happy
      happy `shouldSatisfy` (>) nappy
      happy `shouldSatisfy` (>=) nappy
      max happy nappy `shouldBe` nappy
      min happy nappy `shouldBe` happy
