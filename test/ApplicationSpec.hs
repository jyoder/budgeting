module ApplicationSpec (spec) where

import qualified Application
import qualified Argument
import qualified MockSystem
import qualified Path
import Protolude
import qualified Result
import qualified StateActions
import Test.Hspec

spec :: Spec
spec = do
  describe "run" $ do
    it "prints a helpful usage message when fewer than three command line arguments are given" $ do
      let system = MockSystem.make (Argument.fromText <$> ["arg1", "arg2"]) []
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["Usage: ./budgeting-exe <priorities-csv> <salaries-csv> <teammates-csv>"]
    it "prints a helpful usage message when more than three command line arguments are given" $ do
      let system = MockSystem.make (Argument.fromText <$> ["arg1", "arg2", "arg3", "arg4"]) []
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["Usage: ./budgeting-exe <priorities-csv> <salaries-csv> <teammates-csv>"]
    it "prints an error when the priorities file is not found" $ do
      let system =
            MockSystem.make
              (Argument.fromText <$> ["priorities.csv", "salaries.csv", "teammates.csv"])
              []
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["File not found: priorities.csv"]
    it "prints an error when the priorities file contains invalid data" $ do
      let system =
            MockSystem.make
              (Argument.fromText <$> ["priorities.csv", "salaries.csv", "teammates.csv"])
              [(Path.fromText "priorities.csv", Result.success "invalid priorities data")]
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["priorities.csv: parse error (not enough input) at \"\""]
    it "prints an error when there is an error reading the priorities file" $ do
      let system =
            MockSystem.make
              (Argument.fromText <$> ["priorities.csv", "salaries.csv", "teammates.csv"])
              [(Path.fromText "priorities.csv", Result.error "error reading file")]
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["error reading file"]
    it "prints an error when the salaries file is not found" $ do
      let prioritiesData = "Name,Priority Q1,Priority Q2,Priority Q3,Priority Q4\nRams,Football,Soccer,Football,Ping Pong"
      let system =
            MockSystem.make
              (Argument.fromText <$> ["priorities.csv", "salaries.csv", "teammates.csv"])
              [(Path.fromText "priorities.csv", Result.success prioritiesData)]
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["File not found: salaries.csv"]
    it "prints an error when the salaries file contains invalid data" $ do
      let prioritiesData = "Name,Priority Q1,Priority Q2,Priority Q3,Priority Q4\nRams,Football,Soccer,Football,Ping Pong"
      let system =
            MockSystem.make
              (Argument.fromText <$> ["priorities.csv", "salaries.csv", "teammates.csv"])
              [ (Path.fromText "priorities.csv", Result.success prioritiesData),
                (Path.fromText "salaries.csv", Result.success "invalid salaries data")
              ]
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["salaries.csv: parse error (not enough input) at \"\""]
    it "prints an error when there is an error reading the salaries file" $ do
      let prioritiesData = "Name,Priority Q1,Priority Q2,Priority Q3,Priority Q4\nRams,Football,Soccer,Football,Ping Pong"
      let system =
            MockSystem.make
              (Argument.fromText <$> ["priorities.csv", "salaries.csv", "teammates.csv"])
              [ (Path.fromText "priorities.csv", Result.success prioritiesData),
                (Path.fromText "salaries.csv", Result.error "error reading file")
              ]
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["error reading file"]
    it "prints an error when the teammates file is not found" $ do
      let prioritiesData = "Name,Priority Q1,Priority Q2,Priority Q3,Priority Q4\nRams,Football,Soccer,Football,Ping Pong"
      let salariesData = "Bhc,Name,Salary Q1,Salary Q2,Salary Q3,Salary Q4\n123,Bob,100,110,120,120"
      let system =
            MockSystem.make
              (Argument.fromText <$> ["priorities.csv", "salaries.csv", "teammates.csv"])
              [ (Path.fromText "priorities.csv", Result.success prioritiesData),
                (Path.fromText "salaries.csv", Result.success salariesData)
              ]
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["File not found: teammates.csv"]
    it "prints an error when the teammates file contains invalid data" $ do
      let prioritiesData = "Name,Priority Q1,Priority Q2,Priority Q3,Priority Q4\nRams,Football,Soccer,Football,Ping Pong"
      let salariesData = "Bhc,Name,Salary Q1,Salary Q2,Salary Q3,Salary Q4\n123,Bob,100,110,120,120"
      let system =
            MockSystem.make
              (Argument.fromText <$> ["priorities.csv", "salaries.csv", "teammates.csv"])
              [ (Path.fromText "priorities.csv", Result.success prioritiesData),
                (Path.fromText "salaries.csv", Result.success salariesData),
                (Path.fromText "teammates.csv", Result.success "invalid teammates data")
              ]
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["teammates.csv: parse error (not enough input) at \"\""]
    it "prints an error when there is an error reading the teammates file" $ do
      let prioritiesData = "Name,Priority Q1,Priority Q2,Priority Q3,Priority Q4\nRams,Football,Soccer,Football,Ping Pong"
      let salariesData = "Bhc,Name,Salary Q1,Salary Q2,Salary Q3,Salary Q4\n123,Bob,100,110,120,120"
      let system =
            MockSystem.make
              (Argument.fromText <$> ["priorities.csv", "salaries.csv", "teammates.csv"])
              [ (Path.fromText "priorities.csv", Result.success prioritiesData),
                (Path.fromText "salaries.csv", Result.success salariesData),
                (Path.fromText "teammates.csv", Result.error "error reading file")
              ]
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["error reading file"]
    it "prints an error when validations fail" $ do
      let teammatesData = "Bhc,Name,Teams Q1,Teams Q2,Teams Q3,Teams Q4\n"
      let prioritiesData = "Name,Priority Q1,Priority Q2,Priority Q3,Priority Q4\nRams,Football,Soccer,Football,Ping Pong"
      let salariesData = "Bhc,Name,Salary Q1,Salary Q2,Salary Q3,Salary Q4\n123,Bob,100,110,120,120"
      let system =
            MockSystem.make
              (Argument.fromText <$> ["priorities.csv", "salaries.csv", "teammates.csv"])
              [ (Path.fromText "priorities.csv", Result.success prioritiesData),
                (Path.fromText "salaries.csv", Result.success salariesData),
                (Path.fromText "teammates.csv", Result.success teammatesData)
              ]
      let (_, system') = StateActions.run (Application.run StateActions.make) system
      MockSystem.printed system' `shouldBe` ["Missing BHC \"123\" in teammates file, found on line 1 in salaries file\nMissing team \"Rams\" in teammates file, found on line 1 in priorities file\n"]
