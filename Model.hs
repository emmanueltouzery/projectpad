{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Model where

import Data.Time.Clock
import Data.ByteString
import Database.Persist.TH
import Data.Typeable
import Data.Text
import Graphics.QML
import Database.Persist.Sql
import Data.Int

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

int64to32 :: Int64 -> Int32
int64to32 = fromIntegral

-- TODO generate this with TH?
-- or with some fmap over a list of pairs field name / accessor.
instance DefaultClass (Entity Project) where
	classMembers =
		[
			defPropertyRO "id" (return . int64to32 . fromSqlKey . entityKey . fromObjRef),
			defPropertyRO "name" (return . projectName . entityVal . fromObjRef)
		]

deriving instance Typeable Entity
deriving instance Typeable Key
