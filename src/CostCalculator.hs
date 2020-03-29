module CostCalculator (cost) where

import qualified Money
import Protolude

cost :: Money.T -> Money.T
cost salary = (salary * salaryCostFactor) + perPersonCost

-- Multiplied by salary. Accounts for taxes, bonuses, stock comp, and benefits.
--
-- THIS VALUE NOT REAL. IT IS MADE UP.
salaryCostFactor :: Money.T
salaryCostFactor = 1.50

-- Added to each person's salary. Accounts for legal fees, rent, utilities, QA system, travel
-- and expenses, tech retreat, etc.
--
-- THIS VALUE NOT REAL. IT IS MADE UP.
perPersonCost :: Money.T
perPersonCost = 50000
