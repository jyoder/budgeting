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
          ramsPriority = PriorityRecord.T 1 "Rams" "Football" "Football" "Football" "Football"
          bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
          bobTeammate = TeammateRecord.T 1 "10" "Bob" "Sports" rams rams rams rams
          records = BudgetRecords.T [ramsPriority] [bobSalary] [bobTeammate]
       in ReportGenerator.generate identity records
            `shouldBe` BudgetReport.T [BudgetReport.Row "Football" 100.00 200.00 300.00 400.00]
    it "returns a Q1 spend of $150.00 for a priority with one teammate who makes $100.00 and a second who makes $50.00 in Q1" $ do
      let rams = Teams.make ["Rams"]
          ramsPriority = PriorityRecord.T 1 "Rams" "Football" "Football" "Football" "Football"
          bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
          robSalary = SalaryRecord.T 2 "11" "Rob" 50.00 200.00 300.00 400.00
          bobTeammate = TeammateRecord.T 1 "10" "Bob" "Sports" rams rams rams rams
          robTeammate = TeammateRecord.T 2 "11" "Rob" "Sports" rams rams rams rams
          records = BudgetRecords.T [ramsPriority] [bobSalary, robSalary] [bobTeammate, robTeammate]
       in ReportGenerator.generate identity records
            `shouldBe` BudgetReport.T [BudgetReport.Row "Football" 150.00 400.00 600.00 800.00]
    it "returns a Q1 spend of $100.00 for a priority with one teammate who makes $100.00 and a second who makes $0.00 in Q1" $ do
      let rams = Teams.make ["Rams"]
          ramsPriority = PriorityRecord.T 1 "Rams" "Football" "Football" "Football" "Football"
          bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
          robSalary = SalaryRecord.T 2 "11" "Rob" 0.00 200.00 300.00 400.00
          bobTeammate = TeammateRecord.T 1 "10" "Bob" "Sports" rams rams rams rams
          robTeammate = TeammateRecord.T 2 "11" "Rob" "Sports" rams rams rams rams
          records = BudgetRecords.T [ramsPriority] [bobSalary, robSalary] [bobTeammate, robTeammate]
       in ReportGenerator.generate identity records
            `shouldBe` BudgetReport.T [BudgetReport.Row "Football" 100.00 400.00 600.00 800.00]
    it "returns Q1 spend of $100.00 for Football and $50.00 for Hockey if a teammate in Football makes $100.00 and a teammate in Hockey makes $50.00" $ do
      let (rams, canucks) = (Teams.make ["Rams"], Teams.make ["Canucks"])
          ramsPriority = PriorityRecord.T 1 "Rams" "Football" "Football" "Football" "Football"
          canucksPriority = PriorityRecord.T 1 "Canucks" "Hockey" "Hockey" "Hockey" "Hockey"
          bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
          robSalary = SalaryRecord.T 2 "11" "Rob" 50.00 200.00 300.00 400.00
          bobTeammate = TeammateRecord.T 1 "10" "Bob" "Sports" rams rams rams rams
          robTeammate = TeammateRecord.T 2 "11" "Rob" "Sports" canucks rams rams rams
          records = BudgetRecords.T [ramsPriority, canucksPriority] [bobSalary, robSalary] [bobTeammate, robTeammate]
       in ReportGenerator.generate identity records
            `shouldBe` BudgetReport.T
              [ BudgetReport.Row "Football" 100.00 400.00 600.00 800.00,
                BudgetReport.Row "Hockey" 50.00 0.00 0.00 0.00
              ]
    it "returns the proper spend regardless of the order in which priorities are processed" $ do
      let (rams, canucks) = (Teams.make ["Rams"], Teams.make ["Canucks"])
          ramsPriority = PriorityRecord.T 1 "Rams" "Football" "Football" "Football" "Football"
          canucksPriority = PriorityRecord.T 1 "Canucks" "Hockey" "Hockey" "Hockey" "Hockey"
          bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
          robSalary = SalaryRecord.T 2 "11" "Rob" 50.00 200.00 300.00 400.00
          nobSalary = SalaryRecord.T 3 "12" "Nob" 1.00 2.00 3.00 4.00
          bobTeammate = TeammateRecord.T 1 "10" "Bob" "Sports" rams rams rams rams
          robTeammate = TeammateRecord.T 2 "11" "Rob" "Sports" canucks rams rams rams
          nobTeammate = TeammateRecord.T 3 "12" "Nob" "Sports" rams rams rams rams
          records =
            BudgetRecords.T
              [ramsPriority, canucksPriority]
              [nobSalary, bobSalary, robSalary]
              [bobTeammate, nobTeammate, robTeammate]
       in ReportGenerator.generate identity records
            `shouldBe` BudgetReport.T
              [ BudgetReport.Row "Football" 101.00 402.00 603.00 804.00,
                BudgetReport.Row "Hockey" 50.00 0.00 0.00 0.00
              ]
    it "returns Q1 spend of $50.00 for Football and Hockey if a teammate is split between two teams across the priorities" $ do
      let (rams, ramsCanucks) = (Teams.make ["Rams"], Teams.make ["Rams", "Canucks"])
          ramsPriority = PriorityRecord.T 1 "Rams" "Football" "Football" "Football" "Football"
          canucksPriority = PriorityRecord.T 1 "Canucks" "Hockey" "Hockey" "Hockey" "Hockey"
          bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
          bobTeammate = TeammateRecord.T 2 "10" "Bob" "Sports" ramsCanucks rams rams rams
          records = BudgetRecords.T [ramsPriority, canucksPriority] [bobSalary] [bobTeammate]
       in ReportGenerator.generate identity records
            `shouldBe` BudgetReport.T
              [ BudgetReport.Row "Football" 50.00 200.00 300.00 400.00,
                BudgetReport.Row "Hockey" 50.00 0.00 0.00 0.00
              ]
    it "returns spend for each priority in alphabetical order" $ do
      let (rams, ramsCanucks) = (Teams.make ["Rams"], Teams.make ["Rams", "Canucks"])
          ramsPriority = PriorityRecord.T 2 "Rams" "Football" "Football" "Football" "Football"
          canucksPriority = PriorityRecord.T 1 "Canucks" "Hockey" "Hockey" "Hockey" "Hockey"
          bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
          bobTeammate = TeammateRecord.T 1 "10" "Bob" "Sports" ramsCanucks rams rams rams
          records1 = BudgetRecords.T [ramsPriority, canucksPriority] [bobSalary] [bobTeammate]
          records2 = BudgetRecords.T [canucksPriority, ramsPriority] [bobSalary] [bobTeammate]
       in [ReportGenerator.generate identity records1, ReportGenerator.generate identity records2]
            `shouldMatchList` [ BudgetReport.T
                                  [ BudgetReport.Row "Football" 50.00 200.00 300.00 400.00,
                                    BudgetReport.Row "Hockey" 50.00 0.00 0.00 0.00
                                  ],
                                BudgetReport.T
                                  [ BudgetReport.Row "Football" 50.00 200.00 300.00 400.00,
                                    BudgetReport.Row "Hockey" 50.00 0.00 0.00 0.00
                                  ]
                              ]
    it "returns spend in which the cost function has been applied to each salary" $ do
      let cost salary = (salary + 300.00) * 2.0
          rams = Teams.make ["Rams"]
          ramsPriority = PriorityRecord.T 1 "Rams" "Football" "Football" "Football" "Football"
          bobSalary = SalaryRecord.T 1 "10" "Bob" 100.00 200.00 300.00 400.00
          bobTeammate = TeammateRecord.T 1 "10" "Bob" "Sports" rams rams rams rams
          robSalary = SalaryRecord.T 1 "11" "Rob" 100.00 200.00 300.00 400.00
          robTeammate = TeammateRecord.T 1 "11" "Rob" "Sports" rams rams rams rams
          records = BudgetRecords.T [ramsPriority] [bobSalary, robSalary] [bobTeammate, robTeammate]
       in ReportGenerator.generate cost records
            `shouldBe` BudgetReport.T [BudgetReport.Row "Football" 1600.00 2000.00 2400.00 2800.00]
