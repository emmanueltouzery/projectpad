{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ServerView where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import Database.Esqueleto
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Persist as P

import Model
import ChildEntityCache

data ServerViewState = ServerViewState
	{
		curServerId :: MVar (Maybe Int),
		pois :: EntityListCache ServerPointOfInterest,
		serverWebsites :: EntityListCache ServerWebsite,
		serverDatabases :: EntityListCache ServerDatabase
	} deriving Typeable

instance DynParentHolder ServerViewState where
	dynParentId = curServerId
	clearAllChildrenCaches state = do
		swapMVar_ (pois state) Nothing
		swapMVar_ (serverWebsites state) Nothing
		swapMVar_ (serverDatabases state) Nothing

instance CacheHolder ServerPointOfInterest ServerViewState where
	cacheChildren = pois

instance CacheHolder ServerWebsite ServerViewState where
	cacheChildren = serverWebsites

instance CacheHolder ServerDatabase ServerViewState where
	cacheChildren = serverDatabases

readPois :: Int -> SqlPersistM [Entity ServerPointOfInterest]
readPois serverId = select $ from $ \p -> do
	where_ (p ^. ServerPointOfInterestServerId ==. val (toSqlKey32 serverId))
	orderBy [asc (p ^. ServerPointOfInterestDesc)]
	return p

addServerPoi :: SqlBackend -> ObjRef ServerViewState
	-> Text -> Text -> Text -> Text -> IO ()
addServerPoi sqlBackend stateRef
	pDesc path txt interestTypeT = do
	let interestType = read $ T.unpack interestTypeT
	addHelper sqlBackend stateRef readPois
		$ ServerPointOfInterest pDesc path txt interestType

updateServerPoi :: SqlBackend -> ObjRef ServerViewState -> ObjRef (Entity ServerPointOfInterest)
	-> Text -> Text -> Text -> Text -> IO (ObjRef (Entity ServerPointOfInterest))
updateServerPoi sqlBackend stateRef poiRef
	pDesc path txt interestTypeT = do
	let interestType = read $ T.unpack interestTypeT
	updateHelper sqlBackend stateRef poiRef readPois pois
		[
			ServerPointOfInterestDesc P.=. pDesc, ServerPointOfInterestPath P.=. path,
			ServerPointOfInterestText P.=. txt,
			ServerPointOfInterestInterestType P.=. interestType
		]

deleteServerPois :: SqlBackend -> ObjRef ServerViewState -> [Int] -> IO ()
deleteServerPois = deleteHelper convertKey readPois

readServerWebsites :: Int -> SqlPersistM [Entity ServerWebsite]
readServerWebsites serverId = select $ from $ \p -> do
	where_ (p ^. ServerWebsiteServerId ==. val (toSqlKey32 serverId))
	orderBy [asc (p ^. ServerWebsiteDesc)]
	return p

addServerWebsite :: SqlBackend -> ObjRef ServerViewState
	-> Text -> Text -> Text -> Text -> IO ()
addServerWebsite sqlBackend stateRef
	pDesc url username password = do
	addHelper sqlBackend stateRef readServerWebsites
		$ ServerWebsite pDesc url username password

updateServerWebsite :: SqlBackend -> ObjRef ServerViewState -> ObjRef (Entity ServerWebsite)
	-> Text -> Text -> Text -> Text -> IO (ObjRef (Entity ServerWebsite))
updateServerWebsite sqlBackend stateRef srvWwwRef
	pDesc url username password = do
	updateHelper sqlBackend stateRef srvWwwRef readServerWebsites serverWebsites
		[
			ServerWebsiteDesc P.=. pDesc, ServerWebsiteUrl P.=. url,
			ServerWebsiteUsername P.=. username,
			ServerWebsitePassword P.=. password
		]

deleteServerWebsites :: SqlBackend -> ObjRef ServerViewState -> [Int] -> IO ()
deleteServerWebsites = deleteHelper convertKey readServerWebsites

readServerDatabases :: Int -> SqlPersistM [Entity ServerDatabase]
readServerDatabases serverId = select $ from $ \p -> do
	where_ (p ^. ServerDatabaseServerId ==. val (toSqlKey32 serverId))
	orderBy [asc (p ^. ServerDatabaseDesc)]
	return p

addServerDatabase :: SqlBackend -> ObjRef ServerViewState
	-> Text -> Text -> Text -> Text -> IO ()
addServerDatabase sqlBackend stateRef
	pDesc name username password = do
	addHelper sqlBackend stateRef readServerDatabases
		$ ServerDatabase pDesc name username password

updateServerDatabase :: SqlBackend -> ObjRef ServerViewState -> ObjRef (Entity ServerDatabase)
	-> Text -> Text -> Text -> Text -> IO (ObjRef (Entity ServerDatabase))
updateServerDatabase sqlBackend stateRef srvDbRef
	pDesc name username password = do
	updateHelper sqlBackend stateRef srvDbRef readServerDatabases serverDatabases
		[
			ServerDatabaseDesc P.=. pDesc, ServerDatabaseName P.=. name,
			ServerDatabaseUsername P.=. username,
			ServerDatabasePassword P.=. password
		]

deleteServerDatabases :: SqlBackend -> ObjRef ServerViewState -> [Int] -> IO ()
deleteServerDatabases = deleteHelper convertKey readServerDatabases

createServerViewState :: SqlBackend -> IO (ObjRef ServerViewState)
createServerViewState sqlBackend = do
	serverViewState <- ServerViewState
		<$> newMVar Nothing
		<*> newMVar Nothing
		<*> newMVar Nothing
		<*> newMVar Nothing
	serverViewClass <- newClass
		[
			defMethod "getPois" (getChildren sqlBackend readPois),
			defMethod "addServerPoi" (addServerPoi sqlBackend),
			defMethod "updateServerPoi" (updateServerPoi sqlBackend),
			defMethod "deleteServerPois" (deleteServerPois sqlBackend),
			defMethod "getServerWebsites" (getChildren sqlBackend readServerWebsites),
			defMethod "addServerWebsite" (addServerWebsite sqlBackend),
			defMethod "updateServerWebsite" (updateServerWebsite sqlBackend),
			defMethod "deleteServerWebsites" (deleteServerWebsites sqlBackend),
			defMethod "getServerDatabases" (getChildren sqlBackend readServerDatabases),
			defMethod "addServerDatabase" (addServerDatabase sqlBackend),
			defMethod "updateServerDatabase" (updateServerDatabase sqlBackend),
			defMethod "deleteServerDatabases" (deleteServerDatabases sqlBackend)
		]
	newObject serverViewClass serverViewState
