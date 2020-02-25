module FileConfig (T (..)) where

import qualified Path
import Protolude

data T
  = T
      { priorityFile :: Path.T,
        salaryFile :: Path.T,
        teammateFile :: Path.T
      }
  deriving (Show)
