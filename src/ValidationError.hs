module ValidationError (T, make, toText) where

import Protolude

newtype T = T Text deriving (Show)

make :: Text -> T
make = T

toText :: T -> Text
toText (T message) = message
