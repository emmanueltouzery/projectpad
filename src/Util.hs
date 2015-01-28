{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
module Util where

import Data.Text (Text)
import Control.Error
import Graphics.QML
import Data.Typeable

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
