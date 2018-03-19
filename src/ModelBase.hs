{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module ModelBase where

import Database.Persist.TH
import Data.Text (Text)
import Data.Typeable

type Password = Text
type IpAddress = Text

data InterestType = PoiApplication | PoiLogFile | PoiConfigFile
                  | PoiCommandToRun | PoiCommandTerminal | PoiBackupArchive
    deriving (Show, Read, Eq, Typeable)
derivePersistField "InterestType"

data ServerAccessType = SrvAccessSsh | SrvAccessRdp | SrvAccessWww | SrvAccessSshTunnel
    deriving (Show, Read, Eq, Typeable)
derivePersistField "ServerAccessType"

-- app would be bug tracking, CI...
data ServerType = SrvDatabase
                | SrvApplication
                | SrvHttpOrProxy
                | SrvMonitoring
                | SrvReporting
    deriving (Show, Read, Eq, Typeable)
derivePersistField "ServerType"

data EnvironmentType = EnvDevelopment | EnvUat | EnvStage | EnvProd
    deriving (Show, Read, Eq, Typeable, Ord)
derivePersistField "EnvironmentType"

data RunOn = RunOnServer | RunOnClient
    deriving (Show, Read, Eq, Typeable, Ord)
derivePersistField "RunOn"
