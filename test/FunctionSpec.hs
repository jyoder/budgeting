module FunctionSpec (spec) where

import qualified CommonSpecs
import qualified Function
import Test.Hspec

spec :: Spec
spec = do
  CommonSpecs.showSpec ("Player" :: Function.T) "T \"Player\""
  CommonSpecs.eqSpec ("Player" :: Function.T) ("Umpire" :: Function.T)
  CommonSpecs.ordSpec ("Player" :: Function.T) ("Zlayer" :: Function.T)
  CommonSpecs.toTextSpec Function.toText ("Player" :: Function.T) "Player"
