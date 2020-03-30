module CsvSpec (spec) where

import qualified Csv
import Data.Csv ((.:), (.=))
import qualified Data.Csv
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
  describe "encode" $ do
    it "encodes a CSV record as text" $ do
      Csv.encode ["Field 1", "Field 2"] [DummyRecord "a" "b"] `shouldBe` "Field 1,Field 2\r\na,b\r\n"

data DummyRecord
  = DummyRecord
      { field1 :: !Text,
        field2 :: !Text
      }
  deriving (Show, Eq)

instance Data.Csv.FromNamedRecord DummyRecord where
  parseNamedRecord m =
    DummyRecord
      <$> m .: "Field 1"
      <*> m .: "Field 2"

instance Data.Csv.ToNamedRecord DummyRecord where
  toNamedRecord DummyRecord {field1, field2} =
    Data.Csv.namedRecord
      [ "Field 1" .= field1,
        "Field 2" .= field2
      ]
