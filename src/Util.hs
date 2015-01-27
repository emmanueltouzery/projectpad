{-# LANGUAGE OverloadedStrings #-}
module Util where

import Data.Text (Text)
import Control.Error

serializeEither :: Either Text Text -> [Text]
serializeEither (Left x) = ["error", x]
serializeEither (Right x) = ["success", x]

serializeEither' :: Either Text () -> [Text]
serializeEither' = serializeEither . fmapR (const "") 
