module TeamsSpec (spec) where

import qualified CommonSpecs
import Data.Csv ((.:))
import qualified Data.Csv
import qualified Data.Vector
import Protolude
import qualified Teams
import Test.Hspec

spec :: Spec
spec = do
  describe "toList" $ do
    it "converts the teams to a list" $ do
      let teams = Teams.make ["Rams", "Bears"]
       in Teams.toList teams `shouldBe` ["Rams", "Bears"]
  describe "FromField" $ do
    it "decodes a single team" $ do
      Data.Csv.decodeByName "Teams\nBears"
        `shouldBe` Right
          ( Data.Vector.fromList ["Teams"],
            Data.Vector.fromList [DummyRecord {teams = Teams.make ["Bears"]}]
          )
    it "decodes multiple teams surrounded by quotes and separated by commas" $ do
      Data.Csv.decodeByName "Teams\n\"Bears,Rams\""
        `shouldBe` Right
          ( Data.Vector.fromList ["Teams"],
            Data.Vector.fromList [DummyRecord {teams = Teams.make ["Bears", "Rams"]}]
          )
    it "replaces invalid input bytes with the standard replacement character U+FFFD" $ do
      Data.Csv.decodeByName "Teams\nBe\x2588"
        `shouldBe` Right
          ( Data.Vector.fromList ["Teams"],
            Data.Vector.fromList [DummyRecord {teams = Teams.make ["Be\xFFFD"]}]
          )
  CommonSpecs.showSpec (Teams.make ["Rams", "Bears"]) "T [T \"Rams\",T \"Bears\"]"
  CommonSpecs.eqSpec (Teams.make ["Rams", "Bears"]) (Teams.make ["Bears"])
  CommonSpecs.ordSpec (Teams.make ["Rams", "Bears"]) (Teams.make ["Rams", "Unicorns"])

newtype DummyRecord = DummyRecord {teams :: Teams.T} deriving (Show, Eq, Ord)

instance Data.Csv.FromNamedRecord DummyRecord where
  parseNamedRecord m = DummyRecord <$> m .: "Teams"
