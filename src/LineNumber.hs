module LineNumber (T, next, toText) where

import Protolude

newtype T = T Integer
  deriving (Show, Eq, Ord)
  deriving newtype (Num)

next :: T -> T
next (T number) = T number + 1

toText :: T -> Text
toText (T number) = show number
