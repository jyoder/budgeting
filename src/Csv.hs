module Csv (Data.Csv.FromNamedRecord, decode) where

import qualified Data.ByteString.Lazy
import qualified Data.Csv
import qualified Data.Text
import qualified Data.Text.Encoding
import Protolude
import qualified Result

decode :: Data.Csv.FromNamedRecord f => Text -> Result.T [f]
decode text = case Data.Csv.decodeByName $ _toLazyByteString text of
  Left errorMessage -> Result.error $ Data.Text.pack errorMessage
  Right records -> Result.success $ toList $ snd records
  where
    _toLazyByteString = Data.ByteString.Lazy.fromStrict . Data.Text.Encoding.encodeUtf8
