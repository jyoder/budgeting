module Teams (T, make) where

import qualified Data.ByteString
import qualified Data.Csv
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Error
import Protolude
import qualified Team

newtype T = T [Team.T]
  deriving (Show, Eq, Ord)

make :: [Team.T] -> T
make = T

instance Data.Csv.FromField T where
  parseField s = pure $ Teams.make $ toTeam <$> splitOnComma s
    where
      toTeam = Team.make . Encoding.decodeUtf8With Error.lenientDecode
      splitOnComma = Data.ByteString.split comma
      comma = fromIntegral $ ord ','
