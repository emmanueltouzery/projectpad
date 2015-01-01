{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module ProjectView where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import Database.Esqueleto
import qualified Database.Persist as P
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Maybe

import ModelBase
import Model
import ChildEntityCache

data ProjectViewState = ProjectViewState
	{
		curProjectId :: MVar (Maybe Int),
		servers :: MVar (Maybe [ObjRef (Entity Server)]),
		pois :: MVar (Maybe [ObjRef (Entity ProjectPointOfInterest)])
	} deriving Typeable

instance DynParentHolder ProjectViewState where
	dynParentId = curProjectId
	clearAllChildrenCaches state = do
		swapMVar_ (servers state) Nothing
		swapMVar_ (pois state) Nothing

instance CacheHolder Server ProjectViewState where
	cacheChildren = servers

instance CacheHolder ProjectPointOfInterest ProjectViewState where
	cacheChildren = pois

readServers :: Int -> SqlPersistM [Entity Server]
readServers projectId = select $ from $ \s -> do
	where_ (s ^. ServerProjectId ==. val (toSqlKey32 projectId))
	orderBy [asc (s ^. ServerDesc)]
	return s

addServer :: SqlBackend -> ObjRef ProjectViewState
	-> Text -> IpAddress -> Text -> Text -> Text -> Text -> IO ()
addServer sqlBackend stateRef
	sDesc ipAddr username password serverTypeT serverAccessTypeT = do
	pId <- getCurParentId stateRef
	let pidKey = toSqlKey $ fromIntegral pId
	let srvType = read $ T.unpack serverTypeT
	let srvAccessType = read $ T.unpack serverAccessTypeT
	let server = Server sDesc ipAddr username password srvType srvAccessType pidKey
	runSqlBackend sqlBackend $ P.insert server
	updateCacheQuery sqlBackend stateRef readServers

updateServer :: SqlBackend -> ObjRef ProjectViewState -> ObjRef (Entity Server)
	-> Text -> IpAddress -> Text -> Text -> Text -> Text -> IO (ObjRef (Entity Server))
updateServer sqlBackend stateRef serverRef
	sDesc ipAddr username password serverTypeT serverAccessTypeT = do
	let srvType = read $ T.unpack serverTypeT
	let srvAccessType = read $ T.unpack serverAccessTypeT
	let idKey = entityKey $ fromObjRef serverRef
	runSqlBackend sqlBackend $ P.update idKey
		[
			ServerDesc P.=. sDesc, ServerIp P.=. ipAddr,
			ServerUsername P.=. username, ServerPassword P.=. password,
			ServerType P.=. srvType, ServerAccessType P.=. srvAccessType
		]
	updateCacheQuery sqlBackend stateRef readServers
	newServerList <- fromMaybe (error "No servers after update?")
		<$> (readMVar $ servers (fromObjRef stateRef))
	let mUpdatedServerEntity = find ((== idKey) . entityKey . fromObjRef) newServerList
	return $ fromMaybe (error "Can't find server after update?") mUpdatedServerEntity

readPois :: Int -> SqlPersistM [Entity ProjectPointOfInterest]
readPois projectId = select $ from $ \poi -> do
	where_ (poi ^. ProjectPointOfInterestProjectId ==. val (toSqlKey32 projectId))
	orderBy [asc (poi ^. ProjectPointOfInterestDesc)]
	return poi

addProjectPoi :: SqlBackend -> ObjRef ProjectViewState
	-> Text -> Text -> Text -> Text -> IO ()
addProjectPoi sqlBackend stateRef
	pDesc path txt interestTypeT = do
	let interestType = read $ T.unpack interestTypeT
	pId <- getCurParentId stateRef
	let pidKey = toSqlKey $ fromIntegral pId
	let poi = ProjectPointOfInterest pDesc path txt interestType pidKey
	runSqlBackend sqlBackend $ P.insert poi
	updateCacheQuery sqlBackend stateRef readPois

updateProjectPoi :: SqlBackend -> ObjRef ProjectViewState -> ObjRef (Entity ProjectPointOfInterest)
	-> Text -> Text -> Text -> Text -> IO (ObjRef (Entity ProjectPointOfInterest))
updateProjectPoi sqlBackend stateRef poiRef
	pDesc path txt interestTypeT = do
	let interestType = read $ T.unpack interestTypeT
	let idKey = entityKey $ fromObjRef poiRef
	runSqlBackend sqlBackend $ P.update idKey
		[
			ProjectPointOfInterestDesc P.=. pDesc, ProjectPointOfInterestPath P.=. path,
			ProjectPointOfInterestText P.=. txt,
			ProjectPointOfInterestInterestType P.=. interestType
		]
	updateCacheQuery sqlBackend stateRef readPois
	newPoiList <- fromMaybe (error "No pois after update?")
		<$> (readMVar $ pois (fromObjRef stateRef))
	let mUpdatedPoiEntity = find ((== idKey) . entityKey . fromObjRef) newPoiList
	return $ fromMaybe (error "Can't find poid after update?") mUpdatedPoiEntity

createProjectViewState :: SqlBackend -> IO (ObjRef ProjectViewState)
createProjectViewState sqlBackend = do
	projectViewState <- ProjectViewState
		<$> newMVar Nothing
		<*> newMVar Nothing
		<*> newMVar Nothing
	projectViewClass <- newClass
		[
			defMethod "getServers" (getChildren sqlBackend readServers),
			defMethod "addServer" (addServer sqlBackend),
			defMethod "updateServer" (updateServer sqlBackend),
			defMethod "getPois" (getChildren sqlBackend readPois),
			defMethod "addProjectPoi" (addProjectPoi sqlBackend),
			defMethod "updateProjectPoi" (updateProjectPoi sqlBackend)
		]
	newObject projectViewClass projectViewState
