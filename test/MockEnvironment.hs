module MockEnvironment (T, make, getArguments) where

import qualified Argument

newtype T = T [Argument.T]

make :: [Argument.T] -> T
make = T

getArguments :: T -> [Argument.T]
getArguments (T arguments) = arguments
