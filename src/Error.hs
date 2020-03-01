module Error (T, make, toText, prepend) where

import Protolude

newtype T = T Message deriving (Show)

newtype Message = Message Text deriving (Show)

make :: Text -> T
make = T . Message

toText :: T -> Text
toText (T (Message text)) = text

prepend :: Text -> T -> T
prepend context err = make $ context <> toText err
