module IoActions (make) where

import qualified Actions
import qualified Argument
import qualified Control.Monad.Except as Except
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Error
import qualified Error
import qualified FileFormat
import qualified Path
import Protolude
import qualified Result

make :: Actions.T IO
make = Actions.T getArguments read IoActions.print

getArguments :: IO [Argument.T]
getArguments = do
  args <- getArgs
  return $ Argument.fromText . Data.Text.pack <$> args

read :: Path.T -> Except.ExceptT Error.T IO Text
read = ExceptT . read'

read' :: Path.T -> IO (Result.T Text)
read' path = do
  fileData <- try $ ByteString.readFile filePath :: IO (Either IOException ByteString.ByteString)
  return $ either (Result.error . show) (Result.success . decode) fileData
  where
    filePath = Path.toFilePath path

decode :: ByteString.ByteString -> Text
decode =
  Encoding.decodeUtf8With Error.lenientDecode
    . ByteString.toStrict
    . FileFormat.dropByteOrderMark

print :: Text -> IO ()
print = putStrLn
