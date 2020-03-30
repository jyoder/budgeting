module BhcSpec (spec) where

import qualified Bhc
import qualified CommonSpecs
import Test.Hspec

spec :: Spec
spec = do
  CommonSpecs.showSpec ("123" :: Bhc.T) "T \"123\""
  CommonSpecs.eqSpec ("123" :: Bhc.T) ("456" :: Bhc.T)
  CommonSpecs.ordSpec ("123" :: Bhc.T) ("456" :: Bhc.T)
  CommonSpecs.toTextSpec Bhc.toText ("123" :: Bhc.T) "123"
