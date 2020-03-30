module FileConfigSpec (spec) where

import qualified CommonSpecs
import qualified FileConfig
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to the fields in the record" $ do
      let config = FileConfig.T "a" "b" "c"
      FileConfig.prioritiesFile config `shouldBe` "a"
      FileConfig.salariesFile config `shouldBe` "b"
      FileConfig.teammatesFile config `shouldBe` "c"
  CommonSpecs.showSpec (FileConfig.T "a" "b" "c") "T {prioritiesFile = T \"a\", salariesFile = T \"b\", teammatesFile = T \"c\"}"
