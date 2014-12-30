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

updateServersCache :: CacheHolder Server a => SqlBackend -> ObjRef a -> IO ()
updateServersCache sqlBackend stateRef = do
	pId <- getCurParentId stateRef
	newServers <- runSqlBackend sqlBackend (readServers pId)
	updateCache stateRef newServers
	return ()

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
	updateServersCache sqlBackend stateRef

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
	updateServersCache sqlBackend stateRef
	newServerList <- fromMaybe (error "No servers after update?")
		<$> (readMVar $ servers (fromObjRef stateRef))
	let mUpdatedServerEntity = find ((== idKey) . entityKey . fromObjRef) newServerList
	return $ fromMaybe (error "Can't find server after update?") mUpdatedServerEntity

readPois :: Int -> SqlPersistM [Entity ProjectPointOfInterest]
readPois projectId = select $ from $ \poi -> do
	where_ (poi ^. ProjectPointOfInterestProjectId ==. val (toSqlKey32 projectId))
	orderBy [asc (poi ^. ProjectPointOfInterestDesc)]
	return poi

updatePoisCache :: CacheHolder ProjectPointOfInterest a => SqlBackend -> ObjRef a -> IO ()
updatePoisCache sqlBackend stateRef = do
	pId <- getCurParentId stateRef
	newPois <- runSqlBackend sqlBackend (readPois pId)
	updateCache stateRef newPois
	return ()

addProjectPoi :: SqlBackend -> ObjRef ProjectViewState
	-> Text -> Text -> Text -> Text -> IO ()
addProjectPoi sqlBackend stateRef
	pDesc path txt interestTypeT = do
	let interestType = read $ T.unpack interestTypeT
	pId <- getCurParentId stateRef
	let pidKey = toSqlKey $ fromIntegral pId
	let poi = ProjectPointOfInterest pDesc path txt interestType pidKey
	runSqlBackend sqlBackend $ P.insert poi
	updatePoisCache sqlBackend stateRef

createProjectViewState :: SqlBackend -> IO (ObjRef ProjectViewState)
createProjectViewState sqlBackend = do
	projectViewState <- ProjectViewState
		<$> newMVar Nothing
		<*> newMVar Nothing
		<*> newMVar Nothing
	let ioReadServers = \pId -> runSqlBackend sqlBackend (readServers pId)
	let ioReadPois = \pId -> runSqlBackend sqlBackend (readPois pId)
	projectViewClass <- newClass
		[
			defMethod "getServers" (getChildren ioReadServers),
			defMethod "addServer" (addServer sqlBackend),
			defMethod "updateServer" (updateServer sqlBackend),
			defMethod "getPois" (getChildren ioReadPois),
			defMethod "addProjectPoi" (addProjectPoi sqlBackend)
		]
	newObject projectViewClass projectViewState
