{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, DeriveDataTypeable #-}
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

instance DefaultClass ServerSearchMatch where
	classMembers =
		[
			defPropertyConst "server" (return . smServer . fromObjRef)
		]

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

-- TODO don't manage to specify the type signature for this...
-- getByIds :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) =>
--             EntityField a (Key a) -> [Key a] -> SqlPersistM [Entity a]
getByIds keySelector ids = select $ from $ \s -> do
  where_ (s ^. keySelector `in_` valList ids)
  return s

getProjectSearchMatch :: [Entity Server] -> [Entity ProjectPointOfInterest]
                      -> ObjRef (Entity Project) -> IO (ObjRef ProjectSearchMatch)
getProjectSearchMatch allServers allProjectPois project = do
  let projectKey = entityKey (fromObjRef project)
  let projectServers = filter (\s -> serverProjectId (entityVal s) == projectKey) allServers
  projectServersDC <- mapM newObjectDC projectServers
  let serverSearchMatch = (\s -> ServerSearchMatch s [] [] []) <$> projectServersDC
  serverSearchMatchDC <- mapM newObjectDC serverSearchMatch
  let projectPois = filter (\p -> projectPointOfInterestProjectId (entityVal p) == projectKey) allProjectPois
  projectPoisDC <- mapM newObjectDC projectPois
  newObjectDC $ ProjectSearchMatch project projectPoisDC serverSearchMatchDC

searchText :: SqlBackend -> Text -> IO [ObjRef ProjectSearchMatch]
searchText sqlBackend txt = do
	let query = (%) ++. val txt ++. (%)
        let runQ f = runSqlBackend sqlBackend (f query)
	projects <- runQ filterProjects
        allProjectPois <- runQ filterProjectPois
        servers <- runQ filterServers
        serverWebsites <- runQ filterServerWebsites

        -- start by the leaves of the tree, and go up,
        -- to catch all the cases.
        let serverServerIds = Set.fromList $ entityKey <$> servers
        let serverWebsitesServerIds = Set.fromList $ serverWebsiteServerId . entityVal <$> serverWebsites
        let allServerIds = Set.unions [serverServerIds, serverWebsitesServerIds]

        let projectProjectIds = Set.fromList $ entityKey <$> projects
        allServers <- runSqlBackend sqlBackend
                      (getByIds ServerId $ Set.toList allServerIds)
        let serverProjectIds = Set.fromList $ serverProjectId . entityVal <$> allServers
        let allProjectIds = Set.unions [projectProjectIds, serverProjectIds]
        allProjects <- runSqlBackend sqlBackend (getByIds ProjectId $ Set.toList allProjectIds)
	projectRefs <- mapM newObjectDC allProjects

	mapM (getProjectSearchMatch allServers allProjectPois) projectRefs
