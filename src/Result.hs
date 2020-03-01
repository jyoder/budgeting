module Result (T, error, success, prepend) where

import qualified Error
import Protolude

type T success = Either Error.T success

success :: success -> T success
success = Right

error :: Text -> T success
error errorText = Left (Error.make errorText)

prepend :: Text -> Error.T -> T success
prepend context err = Left $ Error.prepend context err
