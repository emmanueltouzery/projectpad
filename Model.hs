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
	desc Text
	ip IpAddress
	username Text
	password Password
	type ServerType
	projectId ProjectId
	deriving Show Typeable
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

toSqlKey32 :: ToBackendKey SqlBackend record => Int -> Key record
toSqlKey32 = toSqlKey . fromIntegral

getStandardClassMembers :: (Marshal tr, ToBackendKey SqlBackend record, Typeable record,
	 MarshalMode tr ICanReturnTo () ~ Yes) =>
	[(String, record -> tr)] -> [Member (GetObjType (ObjRef (Entity record)))]
getStandardClassMembers pairs = (idProperty:others)
	where
		idProperty = defPropertyRO "id" (return . int64to32 . fromSqlKey . entityKey . fromObjRef)
		others = fmap (\(name, f) -> defPropertyRO name (return . f . entityVal . fromObjRef)) pairs

-- TODO generate this with TH?
instance DefaultClass (Entity Project) where
	classMembers = getStandardClassMembers [("name", projectName)]

instance DefaultClass (Entity Server) where
	classMembers = getStandardClassMembers [("desc", serverDesc)]

deriving instance Typeable Entity
deriving instance Typeable Key

runSqlBackend :: SqlBackend -> SqlPersistM a -> IO a
runSqlBackend = flip runSqlPersistM

-- Signals TODO get rid of this? ##########
data ListChanged deriving Typeable

instance SignalKeyClass ListChanged where
    type SignalParams ListChanged = IO ()
