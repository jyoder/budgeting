module StateActions (make, run) where

import qualified Actions
import qualified Argument
import qualified Control.Monad.Except as CME
import qualified Control.Monad.State.Lazy as CMSL
import qualified Error
import qualified MockSystem
import qualified Path
import Protolude

type ActionState = CMSL.State MockSystem.T

make :: Actions.T ActionState
make = Actions.T getArguments read print'

run :: ActionState a -> MockSystem.T -> (a, MockSystem.T)
run = CMSL.runState

getArguments :: ActionState [Argument.T]
getArguments = do
  MockSystem.getArguments <$> CMSL.get

read :: Path.T -> CME.ExceptT Error.T ActionState Text
read path = do
  system <- CMSL.get
  CME.liftEither $ MockSystem.read system path

print' :: Text -> ActionState ()
print' text = do
  system <- CMSL.get
  CMSL.put $ MockSystem.print system text
