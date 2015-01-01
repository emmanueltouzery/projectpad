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
import Data.Maybe
import Data.List

import Model
import ChildEntityCache

data ServerViewState = ServerViewState
	{
		curServerId :: MVar (Maybe Int),
		pois :: MVar (Maybe [ObjRef (Entity ServerPointOfInterest)])
	} deriving Typeable

instance DynParentHolder ServerViewState where
	dynParentId = curServerId
	clearAllChildrenCaches state = swapMVar_ (pois state) Nothing

instance CacheHolder ServerPointOfInterest ServerViewState where
	cacheChildren = pois

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
	pId <- getCurParentId stateRef
	let pidKey = toSqlKey $ fromIntegral pId
	let poi = ServerPointOfInterest pDesc path txt interestType pidKey
	runSqlBackend sqlBackend $ P.insert poi
	updateCacheQuery sqlBackend stateRef readPois

updateServerPoi :: SqlBackend -> ObjRef ServerViewState -> ObjRef (Entity ServerPointOfInterest)
	-> Text -> Text -> Text -> Text -> IO (ObjRef (Entity ServerPointOfInterest))
updateServerPoi sqlBackend stateRef poiRef
	pDesc path txt interestTypeT = do
	let interestType = read $ T.unpack interestTypeT
	let idKey = entityKey $ fromObjRef poiRef
	runSqlBackend sqlBackend $ P.update idKey
		[
			ServerPointOfInterestDesc P.=. pDesc, ServerPointOfInterestPath P.=. path,
			ServerPointOfInterestText P.=. txt,
			ServerPointOfInterestInterestType P.=. interestType
		]
	updateCacheQuery sqlBackend stateRef readPois
	newPoiList <- fromMaybe (error "No pois after update?")
		<$> (readMVar $ pois (fromObjRef stateRef))
	let mUpdatedPoiEntity = find ((== idKey) . entityKey . fromObjRef) newPoiList
	return $ fromMaybe (error "Can't find poid after update?") mUpdatedPoiEntity

deleteServerPois :: SqlBackend -> ObjRef ServerViewState -> [Int] -> IO ()
deleteServerPois sqlBackend stateRef serverPoiIds = do
	let keys = fmap (toSqlKey . fromIntegral) serverPoiIds :: [Key ServerPointOfInterest]
	mapM_ (\k -> runSqlBackend sqlBackend $ P.delete k) keys
	updateCacheQuery sqlBackend stateRef readPois

createServerViewState :: SqlBackend -> IO (ObjRef ServerViewState)
createServerViewState sqlBackend = do
	serverViewState <- ServerViewState
		<$> newMVar Nothing
		<*> newMVar Nothing
	serverViewClass <- newClass
		[
			defMethod "getPois" (getChildren sqlBackend readPois),
			defMethod "addServerPoi" (addServerPoi sqlBackend),
			defMethod "updateServerPoi" (updateServerPoi sqlBackend),
			defMethod "deleteServerPois" (deleteServerPois sqlBackend)
		]
	newObject serverViewClass serverViewState
