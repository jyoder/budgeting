module Path (T, toText, fromText, toFilePath) where

import qualified Data.Text
import Protolude

newtype T = T Text deriving (Show)

toText :: T -> Text
toText (T text) = text

fromText :: Text -> T
fromText = T

toFilePath :: T -> FilePath
toFilePath = Data.Text.unpack . toText
