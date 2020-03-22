module Csv (Data.Csv.FromNamedRecord, decode) where

import qualified Data.ByteString.Lazy
import qualified Data.Csv
import qualified Data.Text
import qualified Data.Text.Encoding
import Protolude
import qualified Result

decode :: Data.Csv.FromNamedRecord f => Text -> Result.T [f]
decode text = case Data.Csv.decodeByName $ toLazyByteString text of
  Left errorMessage -> Result.error $ Data.Text.pack errorMessage
  Right records -> Result.success $ toList $ snd records
  where
    toLazyByteString = Data.ByteString.Lazy.fromStrict . Data.Text.Encoding.encodeUtf8
