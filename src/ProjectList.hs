{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
module ProjectList where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import qualified Database.Persist as P
import Data.Typeable
import Database.Esqueleto
import Data.Text (Text)
import Data.List
import Data.Maybe

import Model

data ProjectListState = ProjectListState
	{
		projects :: MVar [ObjRef (Entity Project)]
	} deriving Typeable

readProjects :: SqlPersistM [Entity Project]
readProjects = select $ from $ \p -> do
	orderBy [asc (p ^. ProjectName)]
	return p

reReadProjects :: SqlBackend -> SignalKey (IO ()) -> ObjRef ProjectListState -> IO ()
reReadProjects sqlBackend changeKey state = do
	newProjects <- runSqlBackend sqlBackend readProjects
	-- update the model (also makes sure the new project
	-- is inserted respecting my sorting)
	let newQmlProjects = mapM newObjectDC newProjects
	modifyMVar_ (projects $ fromObjRef state) $ const newQmlProjects
	fireSignal changeKey state

addProject :: SqlBackend -> SignalKey (IO ()) -> ObjRef ProjectListState -> Text -> IO ()
addProject sqlBackend changeKey state txt = do
	runSqlBackend sqlBackend $ P.insert (Project txt "")
	reReadProjects sqlBackend changeKey state

updateProject :: SqlBackend -> SignalKey (IO ()) -> ObjRef ProjectListState
	-> ObjRef (Entity Project) -> Text -> IO (ObjRef (Entity Project))
updateProject sqlBackend changeKey state project name = do
	let idKey = entityKey $ fromObjRef project
	runSqlBackend sqlBackend $ P.update idKey [ProjectName P.=. name]
	reReadProjects sqlBackend changeKey state
	newProjectList <- readMVar $ projects (fromObjRef state)
	let mUpdatedProjectEntity = find ((== idKey) . entityKey . fromObjRef) newProjectList
	return $ fromMaybe (error "Can't find project after update?") mUpdatedProjectEntity

createProjectListState :: SqlBackend -> IO (ObjRef ProjectListState)
createProjectListState sqlBackend = do
	changeKey <- newSignalKey
	rootClass <- newClass
		[
			defPropertySigRO "projects" changeKey
				$ readMVar . projects . fromObjRef,
			defMethod "addProject" (addProject sqlBackend changeKey),
			defMethod "updateProject" (updateProject sqlBackend changeKey)
		]
	prj <- runSqlBackend sqlBackend readProjects
	objPrj <- mapM newObjectDC prj
	projectListState <- ProjectListState <$> newMVar objPrj
	newObject rootClass projectListState
