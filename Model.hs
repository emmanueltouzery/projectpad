{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Model where

import Data.Time.Clock
import Data.ByteString
import Database.Persist.TH
import Data.Typeable
import Data.Text
import Graphics.QML

import ModelBase

mkPersist sqlSettings [persistLowerCase|
CommandToRun
	desc String
	path String
	text String
	serverId ServerId Maybe
	deriving Show
PointOfInterest
	description String
	location FilePath
	interestType InterestType
	serverId ServerId Maybe
	deriving Show
Server
	desc String
	ip IpAddress
	username String
	password Password
	type ServerType
	projectId ProjectId
	deriving Show
Project
	name Text
	icon ByteString
	deriving Show Typeable
DbVersion
	code Int
	upgradeDate UTCTime
	deriving Show
|]

-- TODO generate this with TH?
instance DefaultClass Project where
	classMembers =
		[
			defPropertyRO "name" (return . projectName . fromObjRef)
		]
