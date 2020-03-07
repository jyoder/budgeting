module MockEnvironment (T, make, getArguments) where

import qualified Argument
import Protolude

newtype T = T [Argument.T] deriving (Show)

make :: [Argument.T] -> T
make = T

getArguments :: T -> [Argument.T]
getArguments (T arguments) = arguments
