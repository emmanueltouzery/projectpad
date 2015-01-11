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
import qualified Data.ByteString as BS

import ModelBase
import Model
import ChildEntityCache

data ProjectViewState = ProjectViewState
	{
		curProjectId :: MVar (Maybe Int),
		servers :: EntityListCache Server,
		pois :: EntityListCache ProjectPointOfInterest
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

processAuthKeyInfo :: Text -> IO (Maybe (BS.ByteString, Text))
processAuthKeyInfo keyPath = case T.stripPrefix "file://" keyPath of
	Nothing -> return Nothing
	Just p -> do
		contents <- BS.readFile (T.unpack p)
		return $ Just (contents, last $ T.splitOn "/" p)

addServer :: SqlBackend -> ObjRef ProjectViewState
	-> Text -> IpAddress -> Text -> Text -> Text -> Text -> Text -> IO ()
addServer sqlBackend stateRef sDesc ipAddr username password
		keyPath serverTypeT serverAccessTypeT = do
	let srvType = read $ T.unpack serverTypeT
	let srvAccessType = read $ T.unpack serverAccessTypeT
	authKeyInfo <- processAuthKeyInfo keyPath
	addHelper sqlBackend stateRef readServers
		$ Server sDesc ipAddr username password
			(fst <$> authKeyInfo) (snd <$> authKeyInfo) srvType srvAccessType

updateServer :: SqlBackend -> ObjRef ProjectViewState -> ObjRef (Entity Server)
	-> Text -> IpAddress -> Text -> Text -> Text -> Text -> Text -> IO (ObjRef (Entity Server))
updateServer sqlBackend stateRef serverRef sDesc ipAddr username password
		keyPath serverTypeT serverAccessTypeT = do
	let srvType = read $ T.unpack serverTypeT
	let srvAccessType = read $ T.unpack serverAccessTypeT
	authKeyInfo <- processAuthKeyInfo keyPath
	updateHelper sqlBackend stateRef serverRef readServers servers
		[
			ServerDesc P.=. sDesc, ServerIp P.=. ipAddr,
			ServerUsername P.=. username, ServerPassword P.=. password,
			ServerType P.=. srvType, ServerAccessType P.=. srvAccessType,
			ServerAuthKey P.=. fst <$> authKeyInfo,
			ServerAuthKeyFilename P.=. snd <$> authKeyInfo
		]

deleteServers :: SqlBackend -> ObjRef ProjectViewState -> [Int] -> IO ()
deleteServers = deleteHelper convertKey readServers

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
	updateHelper sqlBackend stateRef poiRef readPois pois
		[
			ProjectPointOfInterestDesc P.=. pDesc, ProjectPointOfInterestPath P.=. path,
			ProjectPointOfInterestText P.=. txt,
			ProjectPointOfInterestInterestType P.=. interestType
		]

deleteProjectPois :: SqlBackend -> ObjRef ProjectViewState -> [Int] -> IO ()
deleteProjectPois = deleteHelper convertKey readPois

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
