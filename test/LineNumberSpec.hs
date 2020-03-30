module LineNumberSpec (spec) where

import qualified CommonSpecs
import qualified LineNumber
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "next" $ do
    it "returns the next successive line number" $ do
      LineNumber.next 123 `shouldBe` 124
  CommonSpecs.showSpec (1 :: LineNumber.T) "T 1"
  CommonSpecs.eqSpec (1 :: LineNumber.T) (2 :: LineNumber.T)
  CommonSpecs.ordSpec (1 :: LineNumber.T) (2 :: LineNumber.T)
  CommonSpecs.numSpec (1 :: LineNumber.T) (2 :: LineNumber.T)
  CommonSpecs.toTextSpec LineNumber.toText (1 :: LineNumber.T) "1"
