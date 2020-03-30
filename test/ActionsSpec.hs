module ActionsSpec (spec) where

import qualified Actions
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "accessors" $ do
    it "provides access to fields in the record" $ do
      let actions = Actions.T (Identity ["mom"]) (\_ -> lift $ Identity "Hello") (\_ -> Identity ())
      Actions.getArguments actions `shouldBe` Identity ["mom"]
      Actions.read actions "some/path" `shouldBe` ExceptT (Identity $ Right ("Hello" :: Text))
      Actions.print actions "Barf" `shouldBe` Identity ()
