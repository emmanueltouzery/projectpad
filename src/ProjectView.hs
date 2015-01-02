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
	let srvType = read $ T.unpack serverTypeT
	let srvAccessType = read $ T.unpack serverAccessTypeT
	addHelper sqlBackend stateRef readServers
		$ Server sDesc ipAddr username password srvType srvAccessType

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

deleteServers :: SqlBackend -> ObjRef ProjectViewState -> [Int] -> IO ()
deleteServers sqlBackend stateRef serverIds = do
	let keys = fmap (toSqlKey . fromIntegral) serverIds :: [Key Server]
	mapM_ (\k -> runSqlBackend sqlBackend $ P.delete k) keys
	updateCacheQuery sqlBackend stateRef readServers

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
	addHelper sqlBackend stateRef readPois
		$ ProjectPointOfInterest pDesc path txt interestType

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

deleteProjectPois :: SqlBackend -> ObjRef ProjectViewState -> [Int] -> IO ()
deleteProjectPois sqlBackend stateRef projectPoiIds = do
	let keys = fmap (toSqlKey . fromIntegral) projectPoiIds :: [Key ProjectPointOfInterest]
	mapM_ (\k -> runSqlBackend sqlBackend $ P.delete k) keys
	updateCacheQuery sqlBackend stateRef readPois

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
			defMethod "deleteServers" (deleteServers sqlBackend),
			defMethod "getPois" (getChildren sqlBackend readPois),
			defMethod "addProjectPoi" (addProjectPoi sqlBackend),
			defMethod "updateProjectPoi" (updateProjectPoi sqlBackend),
			defMethod "deleteProjectPois" (deleteProjectPois sqlBackend)
		]
	newObject projectViewClass projectViewState
