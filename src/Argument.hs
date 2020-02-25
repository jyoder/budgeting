module Argument (T, toText, fromText) where

import Protolude

newtype T = T Text

toText :: T -> Text
toText (T text) = text

fromText :: Text -> T
fromText = T
