module CsvSpec (spec) where

import qualified Csv
import Data.Csv ((.:), FromNamedRecord (parseNamedRecord))
import Protolude
import qualified Result
import Test.Hspec

spec :: Spec
spec = do
  describe "decode" $ do
    it "decodes text into a CSV record" $ do
      let dummy = Csv.decode "Field 1,Field 2\na,b" :: Result.T [DummyRecord]
       in dummy `shouldBe` Result.success [DummyRecord {field1 = "a", field2 = "b"}]
    it "fails with an error message when the text cannot be decoded" $ do
      let dummy = Csv.decode "invalid data" :: Result.T [DummyRecord]
       in dummy `shouldBe` Result.error "parse error (not enough input) at \"\""

data DummyRecord
  = DummyRecord
      { field1 :: !Text,
        field2 :: !Text
      }
  deriving (Show, Eq)

instance Csv.FromNamedRecord DummyRecord where
  parseNamedRecord m =
    DummyRecord
      <$> m .: "Field 1"
      <*> m .: "Field 2"
