{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ServerView where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import Database.Esqueleto
import Data.Typeable

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

createServerViewState :: SqlBackend -> IO (ObjRef ServerViewState)
createServerViewState sqlBackend = do
	serverViewState <- ServerViewState
		<$> newMVar Nothing
		<*> newMVar Nothing
	let ioReadPois = \sId -> runSqlBackend sqlBackend (readPointOfInterests sId)
	serverViewClass <- newClass
		[
			defMethod "getPois" (getChildren ioReadPois)
		]
	newObject serverViewClass serverViewState
