module ErrorSpec (spec) where

import qualified Error
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "toText" $ do
    it "converts the error to text" $ do
      let error = Error.make "some error"
      Error.toText error `shouldBe` "some error"
  describe "prepend" $ do
    it "prepends the given text to the error message" $ do
      let error = Error.make "error"
      Error.prepend "some: " error `shouldBe` Error.make "some: error"
  describe "Show" $ do
    it "converts the error to text" $ do
      show (Error.make "error") `shouldBe` ("T (Message \"error\")" :: Text)
  describe "Eq" $ do
    it "tests whether two errors are equal" $ do
      Error.make "error1" `shouldNotBe` Error.make "error2"
