{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
module ProjectList where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import qualified Database.Persist as P
import Data.Typeable
import Database.Esqueleto
import Data.Text (Text)

import Model

data ProjectListState = ProjectListState
	{
		projects :: MVar [ObjRef (Entity Project)]
	} deriving Typeable

readProjects :: SqlPersistM [Entity Project]
readProjects = select $ from $ \p -> do
	orderBy [asc (p ^. ProjectName)]
	return p

addProject :: SqlBackend -> ObjRef ProjectListState -> Text -> IO ()
addProject sqlBackend state text = do
	newProjects <- runSqlBackend sqlBackend $ do
		P.insert (Project text "")
		readProjects
	-- update the model (also makes sure the new project
	-- is inserted respecting my sorting)
	let newQmlProjects = mapM newObjectDC newProjects
	modifyMVar_ (projects $ fromObjRef state) $ const newQmlProjects

createProjectListState :: SqlBackend -> IO (ObjRef ProjectListState)
createProjectListState sqlBackend = do
	rootClass <- newClass
		[
			defPropertyRO "projects"
				$ readMVar . projects . fromObjRef,
			defMethod "addProject" (addProject sqlBackend)
		]
	prj <- runSqlBackend sqlBackend readProjects
	objPrj <- mapM newObjectDC prj
	projectListState <- ProjectListState <$> newMVar objPrj
	newObject rootClass projectListState