module LineNumber (T, fromInt, toInt, toText) where

import Protolude

newtype T = T Int
  deriving (Show, Eq, Ord)
  deriving newtype (Num)

fromInt :: Int -> T
fromInt = T

toInt :: T -> Int
toInt (T lineNumber) = lineNumber

toText :: T -> Text
toText (T lineNumber) = show lineNumber
