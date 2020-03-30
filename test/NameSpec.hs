module NameSpec (spec) where

import qualified CommonSpecs
import qualified Name
import Test.Hspec

spec :: Spec
spec = do
  CommonSpecs.showSpec ("Bob" :: Name.T) "T \"Bob\""
  CommonSpecs.eqSpec ("Bob" :: Name.T) ("Bill" :: Name.T)
  CommonSpecs.ordSpec ("Bill" :: Name.T) ("Bob" :: Name.T)
