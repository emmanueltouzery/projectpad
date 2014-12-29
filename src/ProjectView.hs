{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, ViewPatterns #-}
module ProjectView where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import Database.Esqueleto
import qualified Database.Persist as P
import Data.Typeable
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe

import ModelBase
import Model

data ProjectViewState = ProjectViewState
	{
		curProjectId :: MVar (Maybe Int),
		servers :: MVar [ObjRef (Entity Server)]
	} deriving Typeable

readServers :: Int -> SqlPersistM [Entity Server]
readServers projectId = select $ from $ \s -> do
	where_ (s ^. ServerProjectId ==. val (toSqlKey32 projectId))
	orderBy [asc (s ^. ServerDesc)]
	return s

updateProjectViewCache :: SqlBackend -> ObjRef ProjectViewState -> Int -> IO ()
updateProjectViewCache sqlBackend state_ projectId = do
	let state = fromObjRef state_
	newServers <- runSqlBackend sqlBackend (readServers projectId)
	let newQmlServers = mapM newObjectDC newServers
	modifyMVar_ (curProjectId state) $ const (return $ Just projectId)
	modifyMVar_ (servers state) $ const newQmlServers
	-- maybe i need to fire a signal for hsqml so it updates the objref?

getServers :: SqlBackend -> ObjRef ProjectViewState -> Int -> IO [ObjRef (Entity Server)]
getServers sqlBackend _state projectId = do
	putStrLn "getServers() called!"
	let state = fromObjRef _state
	pId <- readMVar (curProjectId state)
	when (not $ pId == Just projectId) $
		updateProjectViewCache sqlBackend _state projectId
	readMVar $ servers state

getCurProject :: ObjRef ProjectViewState -> IO Int
getCurProject (fromObjRef -> state) = fromMaybe (error "No current project!")
	<$> readMVar (curProjectId state)

addServer :: SqlBackend -> ObjRef ProjectViewState
	-> Text -> IpAddress -> Text -> Text -> Text -> Text -> IO ()
addServer sqlBackend stateRef
	sDesc ipAddr username password serverTypeT serverAccessTypeT = do
	pId <- getCurProject stateRef
	let pidKey = toSqlKey $ fromIntegral pId
	let srvType = read $ T.unpack serverTypeT
	let srvAccessType = read $ T.unpack serverAccessTypeT
	let server = Server sDesc ipAddr username password srvType srvAccessType pidKey
	runSqlBackend sqlBackend $ P.insert server
	updateProjectViewCache sqlBackend stateRef pId

createProjectViewState :: SqlBackend -> IO (ObjRef ProjectViewState)
createProjectViewState sqlBackend = do
	projectViewState <- ProjectViewState
		<$> newMVar Nothing
		<*> newMVar []
	projectViewClass <- newClass
		[
			defMethod "getServers" (getServers sqlBackend),
			defMethod "addServer" (addServer sqlBackend)
		]
	newObject projectViewClass projectViewState
