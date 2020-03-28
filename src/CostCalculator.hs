module CostCalculator (cost) where

import qualified Money
import Protolude

cost :: Money.T -> Money.T
cost salary = (salary * salaryCostFactor) + perPersonCost

-- Multiplied by salary. Accounts for taxes, bonuses, stock comp, and benefits.
salaryCostFactor :: Money.T
salaryCostFactor = 1.0

-- Added to each person's salary. Accounts for legal fees, rent, utilities, QA system, travel
-- and expenses, tech retreat, etc.
perPersonCost :: Money.T
perPersonCost = 0.0
