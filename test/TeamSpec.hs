module TeamSpec (spec) where

import CommonSpecs
import qualified Team
import Test.Hspec

spec :: Spec
spec = do
  CommonSpecs.showSpec ("Bob" :: Team.T) "T \"Bob\""
  CommonSpecs.eqSpec ("Bill" :: Team.T) ("Bob" :: Team.T)
  CommonSpecs.ordSpec ("Bill" :: Team.T) ("Bob" :: Team.T)
