module PathSpec (spec) where

import qualified CommonSpecs
import qualified Path
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "toFilePath" $ do
    it "converts a path to a file path" $ do
      Path.toFilePath ("happy" :: Path.T) `shouldBe` ("happy" :: FilePath)
  CommonSpecs.showSpec ("some/path" :: Path.T) "T \"some/path\""
  CommonSpecs.eqSpec ("happy" :: Path.T) ("sappy" :: Path.T)
  CommonSpecs.ordSpec ("happy" :: Path.T) ("sappy" :: Path.T)
  CommonSpecs.toTextSpec Path.toText ("some/path" :: Path.T) "some/path"
  CommonSpecs.fromTextSpec Path.fromText "some/path" ("some/path" :: Path.T)
