module TeamsSpec (spec) where

import Data.Csv ((.:), FromNamedRecord, decodeByName, parseNamedRecord)
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
      decodeByName "Teams\nBears"
        `shouldBe` Right
          ( Data.Vector.fromList ["Teams"],
            Data.Vector.fromList [DummyRecord {teams = Teams.make ["Bears"]}]
          )
    it "decodes multiple teams surrounded by quotes and separated by commas" $ do
      decodeByName "Teams\n\"Bears,Rams\""
        `shouldBe` Right
          ( Data.Vector.fromList ["Teams"],
            Data.Vector.fromList [DummyRecord {teams = Teams.make ["Bears", "Rams"]}]
          )
    it "replaces invalid input bytes with the standard replacement character U+FFFD" $ do
      decodeByName "Teams\nBe\x2588"
        `shouldBe` Right
          ( Data.Vector.fromList ["Teams"],
            Data.Vector.fromList [DummyRecord {teams = Teams.make ["Be\xFFFD"]}]
          )
  describe "show" $ do
    it "converts teams to text" $ do
      let teams = Teams.make ["Rams", "Bears"]
       in show teams `shouldBe` ("T [T \"Rams\",T \"Bears\"]" :: Text)
  describe "Eq" $ do
    it "returns whether teams are equal" $ do
      let teams1 = Teams.make ["Rams", "Bears"]
      let teams2 = Teams.make ["Bears", "Unicorns"]
      teams1 `shouldNotBe` teams2
  describe "Ord" $ do
    it "tests the ordering of teams" $ do
      let teams1 = Teams.make ["Rams", "Bears"]
      let teams2 = Teams.make ["Rams", "Unicorns"]
      compare teams1 teams2 `shouldBe` LT
      teams2 `shouldSatisfy` (<) teams1
      teams2 `shouldSatisfy` (<=) teams1
      teams1 `shouldSatisfy` (>) teams2
      teams1 `shouldSatisfy` (>=) teams2
      max teams1 teams2 `shouldBe` teams2
      min teams1 teams2 `shouldBe` teams1

newtype DummyRecord = DummyRecord {teams :: Teams.T} deriving (Show, Eq, Ord)

instance Data.Csv.FromNamedRecord DummyRecord where
  parseNamedRecord m = DummyRecord <$> m .: "Teams"
