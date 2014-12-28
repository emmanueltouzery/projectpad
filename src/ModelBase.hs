{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module ModelBase where

import Database.Persist.TH
import Data.Text (Text)
import Data.Typeable

type Password = Text
type IpAddress = Text

data InterestType = PoiApplication | PoiLogFile | PoiDataFile
	deriving (Show, Read, Eq, Typeable)
derivePersistField "InterestType"

-- app would be bug tracking, CI...
data ServerType = SrvSsh | SrvRdp | SrvDatabase | SrvApplication
	deriving (Show, Read, Eq, Typeable)
derivePersistField "ServerType"
