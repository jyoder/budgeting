module PathSpec (spec) where

import qualified Path
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "toText" $ do
    it "convert a path to text" $ do
      Path.toText (Path.fromText "happy") `shouldBe` "happy"
  describe "toText" $ do
    it "convert a path to text" $ do
      Path.toText (Path.fromText "happy") `shouldBe` "happy"
  describe "toFilePath" $ do
    it "converts a path to a file path" $ do
      Path.toFilePath (Path.fromText "happy") `shouldBe` ("happy" :: FilePath)
  describe "Eq" $ do
    it "tests whether two paths are equal" $ do
      Path.fromText "happy" `shouldNotBe` Path.fromText "sappy"
  describe "Ord" $ do
    it "tests the ordering of paths" $ do
      let (happy, nappy) = (Path.fromText "happy", Path.fromText "nappy")
      compare happy nappy `shouldBe` LT
      nappy `shouldSatisfy` (<) happy
      nappy `shouldSatisfy` (<=) happy
      happy `shouldSatisfy` (>) nappy
      happy `shouldSatisfy` (>=) nappy
      max happy nappy `shouldBe` nappy
      min happy nappy `shouldBe` happy
