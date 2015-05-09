{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
module Util where

import Data.Text (Text)
import Control.Error
import Graphics.QML
import Data.Typeable
import Data.Traversable
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Exception

serializeEither :: Either Text Text -> [Text]
serializeEither (Left x) = ["error", x]
serializeEither (Right x) = ["success", x]

serializeEither' :: Either Text () -> [Text]
serializeEither' = serializeEither . fmapR (const "")

data CommandProgress = CommandOutput Text
    | CommandSucceeded
    | CommandFailed Text

eitherToCmdProgress :: Either Text b -> CommandProgress
eitherToCmdProgress (Right _) = CommandSucceeded
eitherToCmdProgress (Left x) = CommandFailed x

cmdProgressToJs :: CommandProgress -> [Text]
cmdProgressToJs (CommandOutput x) = ["text", x]
cmdProgressToJs CommandSucceeded = ["succeeded", ""]
cmdProgressToJs (CommandFailed x) = ["failed", x]

data SignalOutput deriving Typeable
instance SignalKeyClass SignalOutput where
    type SignalParams SignalOutput = [Text] -> IO ()

processAuthKeyInfo :: Text -> IO (Maybe (BS.ByteString, Text))
processAuthKeyInfo keyPath = traverse getInfo $ T.stripPrefix "file://" keyPath
    where getInfo p = do
              contents <- BS.readFile (T.unpack p)
              return (contents, last $ T.splitOn "/" p)

textEx :: SomeException -> Text
textEx = T.pack . show
