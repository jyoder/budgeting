module ArgumentSpec (spec) where

import qualified Argument
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "fromText" $ do
    it "creates a new argument from text" $ do
      let argument = Argument.fromText "argument"
       in Argument.toText argument `shouldBe` "argument"
  describe "toText" $ do
    it "converts an argument to text" $ do
      let argument = Argument.fromText "argument"
       in Argument.toText argument `shouldBe` "argument"
