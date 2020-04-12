module RatioReportGenerator (generate) where

import Protolude
import qualified Quarter
import qualified RatioReport
import qualified Role
import qualified TeammateRecord
import qualified Teams

generate :: [TeammateRecord.T] -> RatioReport.T
generate teammates =
  RatioReport.T
    [ makeRow "Product Manager" teammates,
      makeRow "User Experience Designer" teammates,
      makeRow "Quality Assurance Engineer" teammates
    ]

makeRow :: Role.T -> [TeammateRecord.T] -> RatioReport.Row
makeRow role teammates = row $ ratios role teammates
  where
    row (q1, q2, q3, q4) = RatioReport.Row role q1 q2 q3 q4

ratios :: Role.T -> [TeammateRecord.T] -> (Double, Double, Double, Double)
ratios role teammates = (ratioInQ1, ratioInQ2, ratioInQ3, ratioInQ4)
  where
    ratioInQ1 = ratioInQuarter role Quarter.Q1 teammates
    ratioInQ2 = ratioInQuarter role Quarter.Q2 teammates
    ratioInQ3 = ratioInQuarter role Quarter.Q3 teammates
    ratioInQ4 = ratioInQuarter role Quarter.Q4 teammates

ratioInQuarter :: Role.T -> Quarter.T -> [TeammateRecord.T] -> Double
ratioInQuarter role quarter teammates = ratio seCount roleCount
  where
    roleCount = fromIntegral $ length $ teammatesWithRole role quarter teammates
    seCount = fromIntegral $ length $ teammatesWithRole "Software Engineer" quarter teammates
    ratio num den =
      if den > 0.0
        then num / den
        else errorValue

teammatesWithRole :: Role.T -> Quarter.T -> [TeammateRecord.T] -> [TeammateRecord.T]
teammatesWithRole role quarter = filter (teammateHasRole role quarter)

teammateHasRole :: Role.T -> Quarter.T -> TeammateRecord.T -> Bool
teammateHasRole role quarter teammate =
  TeammateRecord.role teammate == role && onSomeTeam quarter teammate

onSomeTeam :: Quarter.T -> TeammateRecord.T -> Bool
onSomeTeam quarter teammate = Teams.toList (TeammateRecord.teams quarter teammate) /= ["None"]

errorValue :: Double
errorValue = -1.0
