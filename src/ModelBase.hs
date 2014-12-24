{-# LANGUAGE TemplateHaskell #-}
module ModelBase where

import Database.Persist.TH

type Password = String
type IpAddress = String

data InterestType = PoiApplication | PoiLogFile | PoiDataFile
	deriving (Show, Read, Eq)
derivePersistField "InterestType"

-- app would be bug tracking, CI...
data ServerType = SrvSsh | SrvRdp | SrvDatabase | SrvApplication
	deriving (Show, Read, Eq)
derivePersistField "ServerType"
