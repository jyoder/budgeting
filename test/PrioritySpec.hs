module PrioritySpec (spec) where

import qualified CommonSpecs
import qualified Priority
import Test.Hspec

spec :: Spec
spec = do
  CommonSpecs.showSpec ("Drums" :: Priority.T) "T \"Drums\""
  CommonSpecs.eqSpec ("Drums" :: Priority.T) ("Trombone" :: Priority.T)
  CommonSpecs.ordSpec ("Drums" :: Priority.T) ("Trombone" :: Priority.T)
