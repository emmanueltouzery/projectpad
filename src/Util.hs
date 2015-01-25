{-# LANGUAGE OverloadedStrings #-}
module Util where

import Data.Text (Text)

serializeEither :: Either Text Text -> [Text]
serializeEither (Left x) = ["error", x]
serializeEither (Right x) = ["success", x]
