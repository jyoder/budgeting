module Actions (T (..)) where

import qualified Argument
import qualified Control.Monad.Except as Except
import qualified Error
import qualified Path
import Protolude

data T m
  = T
      { getArguments :: m [Argument.T],
        read :: Path.T -> Except.ExceptT Error.T m Text,
        print :: Text -> m ()
      }
