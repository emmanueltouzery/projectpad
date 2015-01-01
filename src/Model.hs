{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Model where

import Data.Time.Clock
import Data.ByteString
import Database.Persist.TH
import Data.Typeable
import Graphics.QML
import Database.Persist.Sql
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T

import ModelBase

-- TODO add DataFile (for instance a key)
mkPersist sqlSettings [persistLowerCase|
ServerPointOfInterest
	desc Text
	path Text
	text Text
	interestType InterestType
	serverId ServerId
	deriving Show Typeable
ProjectPointOfInterest
	desc Text
	path Text
	text Text
	interestType InterestType
	projectId ProjectId
	deriving Show Typeable
Server
	desc Text
	ip IpAddress
	username Text
	password Password
	type ServerType
	accessType ServerAccessType
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
		idProperty = defPropertyConst "id" (return . int64to32 . fromSqlKey . entityKey . fromObjRef)
		others = fmap (\(name, f) -> defPropertyConst name (return . f . entityVal . fromObjRef)) pairs

-- TODO generate this with TH?
instance DefaultClass (Entity Project) where
	classMembers = getStandardClassMembers [("name", projectName)]

text :: Show a => a -> Text
text = T.pack . show

instance DefaultClass (Entity Server) where
	classMembers = getStandardClassMembers [("desc", serverDesc),
		("serverIp", serverIp),
		("username", serverUsername),
		("password", serverPassword),
		("type", text . serverType),
		("accessType", text . serverAccessType),
		("projectId", text . fromSqlKey . serverProjectId)] -- TODO FK as text...

instance DefaultClass (Entity ServerPointOfInterest) where
	classMembers = getStandardClassMembers [
		("desc", serverPointOfInterestDesc),
		("path", serverPointOfInterestPath),
		("text", serverPointOfInterestText),
		("interestType", text . serverPointOfInterestInterestType)]

instance DefaultClass (Entity ProjectPointOfInterest) where
	classMembers = getStandardClassMembers [
		("desc", projectPointOfInterestDesc),
		("path", projectPointOfInterestPath),
		("text", projectPointOfInterestText),
		("interestType", text . projectPointOfInterestInterestType)]

deriving instance Typeable Entity
deriving instance Typeable Key

runSqlBackend :: SqlBackend -> SqlPersistM a -> IO a
runSqlBackend = flip runSqlPersistM
