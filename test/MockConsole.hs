module MockConsole (T, make, MockConsole.print, printed) where

import Protolude

newtype T = T [Text]

make :: T
make = T []

print :: T -> Text -> T
print (T texts) text = T (text : texts)

printed :: T -> [Text]
printed (T texts) = texts
