module Error (T, make, toText) where

import Protolude

newtype T = T Message deriving (Show)

newtype Message = Message Text deriving (Show)

make :: Text -> T
make = T . Message

toText :: T -> Text
toText (T (Message text)) = text
