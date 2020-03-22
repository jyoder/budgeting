module Department (T, toText, excludedFromDev) where

import qualified Data.Csv
import Protolude

newtype T = T Text
  deriving (Show, Eq, Ord)
  deriving newtype (IsString, Data.Csv.FromField)

toText :: T -> Text
toText (T text) = text

excludedFromDev :: T -> Bool
excludedFromDev department = department `elem` nonDev

nonDev :: [T]
nonDev =
  [ T "Web Operations",
    T "Security"
  ]
