module ReportGeneratorSpec (spec) where

import qualified BudgetRecords
import qualified BudgetReport
import qualified PriorityRecord
import Protolude
import qualified ReportGenerator
import qualified SalaryRecord
import qualified TeammateRecord
import qualified Teams
import Test.Hspec

spec :: Spec
spec = do
  describe "generate" $ do
    it "returns a Q1 spend of $100.00 for a priority with a teammate who makes $100.00 in Q1" $ do
      let rams = Teams.make ["Rams"]
      let ramsPriority = PriorityRecord.T 1 "Rams" "Football" "Football" "Football" "Football"
      let bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
      let bobTeammate = TeammateRecord.T 1 "10" "Bob" "Sports" rams rams rams rams
      let records = BudgetRecords.T [ramsPriority] [bobSalary] [bobTeammate]
      ReportGenerator.generate records `shouldBe` BudgetReport.T [BudgetReport.Spend "Football" 100.00]
    it "returns a Q1 spend of $150.00 for a priority with one teammate who makes $100.00 and a second who makes $50.00 in Q1" $ do
      let rams = Teams.make ["Rams"]
      let ramsPriority = PriorityRecord.T 1 "Rams" "Football" "Football" "Football" "Football"
      let bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
      let robSalary = SalaryRecord.T 2 "11" "Rob" 50.00 200.00 300.00 400.00
      let bobTeammate = TeammateRecord.T 1 "10" "Bob" "Sports" rams rams rams rams
      let robTeammate = TeammateRecord.T 1 "11" "Rob" "Sports" rams rams rams rams
      let records = BudgetRecords.T [ramsPriority] [bobSalary, robSalary] [bobTeammate, robTeammate]
      ReportGenerator.generate records `shouldBe` BudgetReport.T [BudgetReport.Spend "Football" 150.00]
    it "returns a Q1 spend of $100.00 for a priority with one teammate who makes $100.00 and a second who makes $0.00 in Q1" $ do
      let rams = Teams.make ["Rams"]
      let ramsPriority = PriorityRecord.T 1 "Rams" "Football" "Football" "Football" "Football"
      let bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
      let robSalary = SalaryRecord.T 2 "11" "Rob" 0.00 200.00 300.00 400.00
      let bobTeammate = TeammateRecord.T 1 "10" "Bob" "Sports" rams rams rams rams
      let robTeammate = TeammateRecord.T 1 "11" "Rob" "Sports" rams rams rams rams
      let records = BudgetRecords.T [ramsPriority] [bobSalary, robSalary] [bobTeammate, robTeammate]
      ReportGenerator.generate records `shouldBe` BudgetReport.T [BudgetReport.Spend "Football" 100.00]
    it "returns Q1 spend of $100.00 for Football and $50.00 for Hockey if a teammate in Football makes $100.00 and a teammate in Hockey makes $50.00" $ do
      let (rams, canucks) = (Teams.make ["Rams"], Teams.make ["Canucks"])
      let ramsPriority = PriorityRecord.T 1 "Rams" "Football" "Football" "Football" "Football"
      let canucksPriority = PriorityRecord.T 1 "Canucks" "Hockey" "Hockey" "Hockey" "Hockey"
      let bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
      let robSalary = SalaryRecord.T 2 "11" "Rob" 50.00 200.00 300.00 400.00
      let bobTeammate = TeammateRecord.T 1 "10" "Bob" "Sports" rams rams rams rams
      let robTeammate = TeammateRecord.T 1 "11" "Rob" "Sports" canucks rams rams rams
      let records = BudgetRecords.T [ramsPriority, canucksPriority] [bobSalary, robSalary] [bobTeammate, robTeammate]
      ReportGenerator.generate records
        `shouldBe` BudgetReport.T
          [ BudgetReport.Spend "Football" 100.00,
            BudgetReport.Spend "Hockey" 50.00
          ]
