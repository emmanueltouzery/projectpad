{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
module ProjectView where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import Database.Esqueleto
import Data.Typeable
import Control.Monad

import Model

data ProjectViewScreenState = ProjectViewScreenState
	{
		curProjectId :: MVar (Maybe Int),
		servers :: MVar [ObjRef (Entity Server)]
	} deriving Typeable

readServers :: Int -> SqlPersistM [Entity Server]
readServers projectId = select $ from $ \s -> do
	where_ (s ^. ServerProjectId ==. val (toSqlKey32 projectId))
	orderBy [asc (s ^. ServerDesc)]
	return s

updateProjectViewCache :: SqlBackend -> ProjectViewScreenState -> Int -> IO ()
updateProjectViewCache sqlBackend state projectId = do
	newServers <- runSqlBackend sqlBackend (readServers projectId)
	let newQmlServers = mapM newObjectDC newServers
	modifyMVar_ (curProjectId state) $ const (return $ Just projectId)
	modifyMVar_ (servers state) $ const newQmlServers
	-- maybe i need to fire a signal for hsqml so it updates the objref?

getServers :: SqlBackend -> ObjRef ProjectViewScreenState -> Int -> IO [ObjRef (Entity Server)]
getServers sqlBackend _state projectId = do
	putStrLn "getServers() called!"
	let state = fromObjRef _state
	pId <- readMVar (curProjectId state)
	when (not $ pId == Just projectId) (updateProjectViewCache sqlBackend state projectId)
	readMVar $ servers state

createProjectViewState :: SqlBackend -> IO (ObjRef ProjectViewScreenState)
createProjectViewState sqlBackend = do
	projectViewState <- ProjectViewScreenState
		<$> newMVar Nothing
		<*> newMVar []
	projectViewClass <- newClass
		[
			defMethod "getServers" (getServers sqlBackend)
		]
	newObject projectViewClass projectViewState
