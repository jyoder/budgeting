module FileConfig (T (..)) where

import qualified Path
import Protolude

data T
  = T
      { prioritiesFile :: Path.T,
        salariesFile :: Path.T,
        teammatesFile :: Path.T
      }
  deriving (Show)
