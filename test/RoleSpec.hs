module RoleSpec (spec) where

import qualified CommonSpecs
import qualified Role
import Test.Hspec

spec :: Spec
spec = do
  CommonSpecs.showSpec ("Player" :: Role.T) "T \"Player\""
  CommonSpecs.eqSpec ("Player" :: Role.T) ("Umpire" :: Role.T)
  CommonSpecs.ordSpec ("Player" :: Role.T) ("Zlayer" :: Role.T)
  CommonSpecs.toTextSpec Role.toText ("Player" :: Role.T) "Player"
