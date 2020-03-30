module ArgumentSpec (spec) where

import qualified Argument
import qualified CommonSpecs
import Test.Hspec

spec :: Spec
spec = do
  CommonSpecs.showSpec ("arg" :: Argument.T) "T \"arg\""
  CommonSpecs.eqSpec ("arg1" :: Argument.T) ("arg2" :: Argument.T)
  CommonSpecs.toTextSpec Argument.toText ("arg" :: Argument.T) "arg"
  CommonSpecs.fromTextSpec Argument.fromText "arg" ("arg" :: Argument.T)
