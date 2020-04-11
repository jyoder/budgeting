module RatioReportGeneratorSpec (spec) where

import Protolude
import qualified RatioReportGenerator
import Test.Hspec

spec :: Spec
spec = do
  describe "generate" $ do
    it "returns a ratio report with a 2:1 ratio when there are two devs and one PM" $ do
      1 `shouldBe` RatioReportGenerator.generate
