module ValidatorSpec (spec) where

import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "toText" $ do
    it "sdf" $ do
      1 `shouldBe` (1 :: Integer)
-- let salaries = Set.fromList [SalaryRecord.T "1" "Bill" 1 1 1 1, SalaryRecord.T "2" "Bob" 1 1 1 1]
-- let (t1, t2, t3, t4) = (Teams.make ["T1"], Teams.make ["T2"], Teams.make ["T3"], Teams.make ["T4"])
-- let teammates = Set.fromList [TeammateRecord.T "1" "Bill" t1 t2 t3 t4]
-- Set.size (Validator.joinSalariesTeammatesOnBhc salaries teammates) `shouldBe` 1
