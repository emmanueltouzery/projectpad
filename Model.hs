{-# LANGUAGE TemplateHaskell #-}
module Model where

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Data.Time.Clock
import Control.Applicative

import SqliteSimpleTH

type Password = String
type IpAddress = String

data CommandToRun = CommandToRun
	{
		commandId :: Int,
		commandDesc :: String,
		commandPath :: String,
		commandText :: String
	}

data InterestType = PoiApplication | PoiLogFile

data PointOfInterest = PointOfInterest
	{
		poiId :: Int,
		poiDescription :: String,
		poiLocation :: FilePath,
		poiInterestType :: InterestType
	}

data ServerType = SrvSsh | SrvRdp | SrvDatabase | SrvApplication

data ServerDetails = ServerDetails
	{
		serverId :: Int,
		serverDesc :: String,
		serverIp :: IpAddress,
		serverUsername :: String,
		serverPassword :: Password,
		serverType :: ServerType,
		serverPois :: [PointOfInterest],
		commandsToRun :: [CommandToRun]
	}

data Project = Project
	{
		projectId :: Int,
		projectName :: String,
		projectServers :: [ServerDetails],
		projectPois :: [PointOfInterest]
	}

data DbVersion = DbVersion
	{
		dbVersionId :: Int,
		versionCode :: Int,
		upgradeDate :: UTCTime
	}
-- $(deriveFromRow ''DbVersion)
-- instance FromRow DbVersion where
-- 	fromRow = do
-- 		f <- field
-- 		g <- field
-- 		h <- field
-- 		return $ DbVersion f g h

--instance ToRow DbVersion where
--	toRow r = [toField (dbVersionId r), toField (versionCode r), toField (upgradeDate r)]
-- $(deriveToRow ''DbVersion)
$(deriveFromToRow ''DbVersion)
