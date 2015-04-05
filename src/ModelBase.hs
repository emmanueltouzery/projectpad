{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module ModelBase where

import Database.Persist.TH
import Data.Text (Text)
import Data.Typeable

type Password = Text
type IpAddress = Text

data InterestType = PoiApplication | PoiLogFile | PoiConfigFile | PoiCommandToRun
	deriving (Show, Read, Eq, Typeable)
derivePersistField "InterestType"

data ServerAccessType = SrvAccessSsh | SrvAccessRdp | SrvAccessWww
	deriving (Show, Read, Eq, Typeable)
derivePersistField "ServerAccessType"

-- app would be bug tracking, CI...
data ServerType = SrvDatabase | SrvApplication
	deriving (Show, Read, Eq, Typeable)
derivePersistField "ServerType"

data EnvironmentType = EnvDevelopment | EnvUat | EnvStage | EnvProd
	deriving (Show, Read, Eq, Typeable, Ord)
derivePersistField "EnvironmentType"
