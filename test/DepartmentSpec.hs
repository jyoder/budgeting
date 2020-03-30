module DepartmentSpec (spec) where

import qualified CommonSpecs
import qualified Department
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "excludedFromDev" $ do
    it "returns true if the department is \"Web Operations\"" $ do
      "Web Operations" `shouldSatisfy` Department.excludedFromDev
    it "returns true if the department is \"Security\"" $ do
      "Security" `shouldSatisfy` Department.excludedFromDev
    it "returns false if the department is not \"Security\" or \"Web Operations\"" $ do
      "Software Engineering" `shouldSatisfy` not . Department.excludedFromDev
  CommonSpecs.showSpec ("Sports" :: Department.T) "T \"Sports\""
  CommonSpecs.eqSpec ("Sports" :: Department.T) ("Cars" :: Department.T)
  CommonSpecs.ordSpec ("Cars" :: Department.T) ("Sports" :: Department.T)
  CommonSpecs.toTextSpec Department.toText ("Sports" :: Department.T) "Sports"
