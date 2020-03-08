module FileFormatSpec (spec) where

import qualified Data.ByteString.Lazy as DBL
import qualified FileFormat
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "dropByteOrderMark" $ do
    it "returns the same string if the byte order mark is not present" $ do
      FileFormat.dropByteOrderMark "hello" `shouldBe` "hello"
    it "returns a string with the byte order mark removed if it was present in the input" $ do
      FileFormat.dropByteOrderMark (DBL.pack [0xEF, 0xBB, 0xBF]) <> "hello" `shouldBe` "hello"
