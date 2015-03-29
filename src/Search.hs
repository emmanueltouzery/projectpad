{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Search where

import Data.Text (Text)
import Data.Typeable
import Database.Esqueleto
import Graphics.QML
import Control.Applicative

import Model

data ProjectSearchMatch = ProjectSearchMatch
	{
		smProject :: ObjRef (Entity Project),
		smProjectPois :: [ObjRef (Entity ProjectPointOfInterest)],
		smServers :: [ObjRef (Entity Server)],
		smServerWebsites :: [ObjRef (Entity ServerWebsite)],
		smServerExtraUsers :: [ObjRef (Entity ServerExtraUserAccount)],
		smServerPoi :: [ObjRef (Entity ServerPointOfInterest)]
	} deriving Typeable

instance DefaultClass ProjectSearchMatch where
	classMembers =
		[
			defPropertyConst "project" (return . smProject . fromObjRef)
		]

filterProjects :: SqlExpr (Value Text) -> SqlPersistM [Entity Project]
filterProjects query = select $ from $ \p -> do
	where_ (p ^. ProjectName `like` query)
	orderBy [asc (p ^. ProjectName)]
	return p

searchText :: SqlBackend -> Text -> IO [ObjRef ProjectSearchMatch]
searchText sqlBackend txt = do
	let query = (%) ++. val txt ++. (%)
	projects <- runSqlBackend sqlBackend (filterProjects query)
	projectRefs <- mapM newObjectDC projects
	mapM newObjectDC $ (\x -> ProjectSearchMatch x [] [] [] [] []) <$> projectRefs
