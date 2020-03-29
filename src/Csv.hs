module Csv (Data.Csv.FromNamedRecord, decode, encode) where

import qualified Data.ByteString.Lazy
import qualified Data.Csv
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Vector
import Protolude
import qualified Result

decode :: Data.Csv.FromNamedRecord r => Text -> Result.T [r]
decode text = case Data.Csv.decodeByName $ toLazyByteString text of
  Left errorMessage -> Result.error $ Data.Text.pack errorMessage
  Right records -> Result.success $ toList $ snd records
  where
    toLazyByteString = Data.ByteString.Lazy.fromStrict . Data.Text.Encoding.encodeUtf8

encode :: Data.Csv.ToNamedRecord r => [Text] -> [r] -> Text
encode headers records = csvText
  where
    csvText = Data.Text.Encoding.decodeUtf8With (\_ _ -> Just '?') strictCsvByteString
    strictCsvByteString = Data.ByteString.Lazy.toStrict csvByteString
    csvByteString = Data.Csv.encodeByName headerVector records
    headerVector = Data.Vector.fromList $ map Data.Text.Encoding.encodeUtf8 headers
