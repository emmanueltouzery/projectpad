{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ProjectView where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import Database.Esqueleto
import qualified Database.Persist as P
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T

import ModelBase
import Model
import ChildEntityCache

data ProjectViewState = ProjectViewState
	{
		curProjectId :: MVar (Maybe Int),
		servers :: MVar [ObjRef (Entity Server)]
	} deriving Typeable

instance DynParentHolder ProjectViewState where
	dynParentId = curProjectId
	clearAllChildrenCaches state = swapMVar_ (servers state) []

instance CacheHolder Server ProjectViewState where
	cacheChildren = servers

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
	newServers <- runSqlBackend sqlBackend (readServers pId)
	updateCache stateRef newServers pId

createProjectViewState :: SqlBackend -> IO (ObjRef ProjectViewState)
createProjectViewState sqlBackend = do
	projectViewState <- ProjectViewState
		<$> newMVar Nothing
		<*> newMVar []
	let ioReadServers = \pId -> runSqlBackend sqlBackend (readServers pId)
	projectViewClass <- newClass
		[
			defMethod "getServers" (getChildren ioReadServers),
			defMethod "addServer" (addServer sqlBackend)
		]
	newObject projectViewClass projectViewState
