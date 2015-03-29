{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Search where

import Data.Text (Text)
import Data.Typeable
import Database.Esqueleto
import Graphics.QML
import Control.Applicative
import qualified Data.Set as Set

import Model

data ServerSearchMatch = ServerSearchMatch
	{
		smServer :: ObjRef (Entity Server),
		smServerWebsites :: [ObjRef (Entity ServerWebsite)],
		smServerExtraUsers :: [ObjRef (Entity ServerExtraUserAccount)],
		smServerPoi :: [ObjRef (Entity ServerPointOfInterest)]
	} deriving Typeable

data ProjectSearchMatch = ProjectSearchMatch
	{
		smProject :: ObjRef (Entity Project),
		smProjectPois :: [ObjRef (Entity ProjectPointOfInterest)],
                smProjectServers :: [ObjRef ServerSearchMatch]
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

filterProjectPois :: SqlExpr (Value Text) -> SqlPersistM [Entity ProjectPointOfInterest]
filterProjectPois query = select $ from $ \p -> do
  where_ ((p ^. ProjectPointOfInterestDesc `like` query)
          ||. (p ^. ProjectPointOfInterestText `like` query)
          ||. (p ^. ProjectPointOfInterestPath `like` query))
  return p

filterServers :: SqlExpr (Value Text) -> SqlPersistM [Entity Server]
filterServers query = select $ from $ \s -> do
  where_ ((s ^. ServerDesc `like` query)
          ||. (s ^. ServerIp `like` query)
          ||. (s ^. ServerText `like` query))
  return s

filterServerWebsites :: SqlExpr (Value Text) -> SqlPersistM [Entity ServerWebsite]
filterServerWebsites query = select $ from $ \w -> do
  where_ ((w ^. ServerWebsiteDesc `like` query)
         ||. (w ^. ServerWebsiteUrl `like` query)
         ||. (w ^. ServerWebsiteText `like` query))
  return w

getServersProjectIds :: [Key Server] -> SqlPersistM [Entity Server]
getServersProjectIds serverIds = select $ from $ \s -> do
  where_ (s ^. ServerId `in_` valList serverIds)
  return s

searchText :: SqlBackend -> Text -> IO [ObjRef ProjectSearchMatch]
searchText sqlBackend txt = do
	let query = (%) ++. val txt ++. (%)
        let runQ f = runSqlBackend sqlBackend (f query)
	projects <- runQ filterProjects
	projectRefs <- mapM newObjectDC projects
        projectPois <- runQ filterProjectPois
        servers <- runQ filterServers
        serverWebsites <- runQ filterServerWebsites

        -- start by the leaves of the tree, and go up,
        -- to catch all the cases.
        let serverServerIds = Set.fromList $ entityKey <$> servers
        let serverWebsitesServerIds = Set.fromList $ serverWebsiteServerId . entityVal <$> serverWebsites
        let allServerIds = Set.unions [serverServerIds, serverWebsitesServerIds]

        let projectProjectIds = Set.fromList $ entityKey <$> projects
        serverProjectIds <- Set.fromList <$> fmap (serverProjectId . entityVal) <$>
                            runSqlBackend sqlBackend (getServersProjectIds $ Set.toList allServerIds)
        let allProjectIds = Set.unions [projectProjectIds, serverProjectIds]

	mapM newObjectDC $ (\x -> ProjectSearchMatch x [] []) <$> projectRefs
