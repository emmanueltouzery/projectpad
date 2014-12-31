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
		pois :: MVar (Maybe [ObjRef (Entity ServerPointOfInterest)])
	} deriving Typeable

instance DynParentHolder ServerViewState where
	dynParentId = curServerId
	clearAllChildrenCaches state = swapMVar_ (pois state) Nothing

instance CacheHolder ServerPointOfInterest ServerViewState where
	cacheChildren = pois

readPointOfInterests :: Int -> SqlPersistM [Entity ServerPointOfInterest]
readPointOfInterests serverId = select $ from $ \p -> do
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
	updateCacheQuery sqlBackend stateRef readPointOfInterests

createServerViewState :: SqlBackend -> IO (ObjRef ServerViewState)
createServerViewState sqlBackend = do
	serverViewState <- ServerViewState
		<$> newMVar Nothing
		<*> newMVar Nothing
	serverViewClass <- newClass
		[
			defMethod "getPois" (getChildren sqlBackend readPointOfInterests),
			defMethod "addServerPoi" (addServerPoi sqlBackend)
		]
	newObject serverViewClass serverViewState
