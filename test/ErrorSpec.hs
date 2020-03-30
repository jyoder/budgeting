module ErrorSpec (spec) where

import qualified CommonSpecs
import qualified Error
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "prepend" $ do
    it "prepends the given text to the error message" $ do
      let error = Error.make "error"
       in Error.prepend "some: " error `shouldBe` Error.make "some: error"
  CommonSpecs.showSpec (Error.make "error") "T (Message \"error\")"
  CommonSpecs.eqSpec (Error.make "error1") (Error.make "error2")
  CommonSpecs.toTextSpec Error.toText (Error.make "error") "error"
