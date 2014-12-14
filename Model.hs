{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
module Model where

import Data.Time.Clock
import Control.Applicative
import Data.ByteString
import Database.Persist.TH

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
	name String
	icon ByteString
	deriving Show
DbVersion
	code Int
	upgradeDate UTCTime
	deriving Show
|]

-- getProjects :: Connection -> IO [Project]
-- getProjects conn = query_ conn "select * from project"
