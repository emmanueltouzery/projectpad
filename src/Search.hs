{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, DeriveDataTypeable, ExistentialQuantification, RankNTypes, FlexibleContexts #-}
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

filterServerExtraUsers :: SqlExpr (Value Text) -> SqlPersistM [Entity ServerExtraUserAccount]
filterServerExtraUsers query = select $ from $ \w -> do
	where_ (w ^. ServerExtraUserAccountDesc `like` query)
	return w

filterServerPois :: SqlExpr (Value Text) -> SqlPersistM [Entity ServerPointOfInterest]
filterServerPois query = select $ from $ \w -> do
	where_ ((w ^. ServerPointOfInterestDesc `like` query)
		||. (w ^. ServerPointOfInterestPath `like` query)
		||. (w ^. ServerPointOfInterestText `like` query))
	return w

-- TODO don't manage to specify the type signature for this...
-- getByIds :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) =>
--             EntityField a (Key a) -> [Key a] -> SqlPersistM [Entity a]
getByIds keySelector ids = select $ from $ \s -> do
	where_ (s ^. keySelector `in_` valList ids)
	return s

--filterEntityJoin :: Key Server -> ServerJoin record -> [Entity record]
filterEntityJoin parentKey (Join fieldGetter entities) = mapM newObjectDC $ filter (\e -> fieldGetter (entityVal e) == parentKey) entities

getServerSearchMatch :: ServerJoin ServerWebsite -> ServerJoin ServerExtraUserAccount -> ServerJoin ServerPointOfInterest
                     -> ObjRef (Entity Server) -> IO (ObjRef ServerSearchMatch)
getServerSearchMatch serverWebsitesJoin serverExtraUsersJoin serverPoisJoin  server = do
	let serverKey = entityKey (fromObjRef server)
	newObjectDC =<< ServerSearchMatch server
		<$> filterEntityJoin serverKey serverWebsitesJoin
		<*> filterEntityJoin serverKey serverExtraUsersJoin
		<*> filterEntityJoin serverKey serverPoisJoin

getProjectSearchMatch :: ProjectJoin Server -> ServerJoin ServerWebsite -> ServerJoin ServerExtraUserAccount
	-> ServerJoin ServerPointOfInterest -> ProjectJoin ProjectPointOfInterest
	-> ObjRef (Entity Project) -> IO (ObjRef ProjectSearchMatch)
getProjectSearchMatch projectServersJoin serverWebsitesJoin serverExtraUsersJoin serverPoisJoin projectPoisJoin project = do
	let projectKey = entityKey (fromObjRef project)
	serverSearchMatch <- mapM (getServerSearchMatch serverWebsitesJoin serverExtraUsersJoin serverPoisJoin )
		<$> filterEntityJoin projectKey projectServersJoin
	newObjectDC =<< ProjectSearchMatch project
		<$> filterEntityJoin projectKey projectPoisJoin
		<*> serverSearchMatch

data Join a b = Join (a -> Key b) [Entity a]
type ServerJoin a = Join a Server
type ProjectJoin a = Join a Project

setFromFieldVal :: Ord (Key b) => Join a b -> Set.Set (Key b)
setFromFieldVal (Join fieldGetter entities) = Set.fromList $ fieldGetter . entityVal <$> entities

searchText :: SqlBackend -> Text -> IO [ObjRef ProjectSearchMatch]
searchText sqlBackend txt = do
	let query = (%) ++. val txt ++. (%)
	let runQ f = runSqlBackend sqlBackend (f query)
	projects <- runQ filterProjects
	projectPoisJoin <- Join projectPointOfInterestProjectId <$> runQ filterProjectPois
	servers <- runQ filterServers
	serverWebsitesJoin <- Join serverWebsiteServerId <$> runQ filterServerWebsites
	serverExtraUsersJoin <- Join serverExtraUserAccountServerId <$> runQ filterServerExtraUsers
	serverPoisJoin <- Join serverPointOfInterestServerId <$> runQ filterServerPois

	-- start by the leaves of the tree, and go up,
	-- to catch all the cases.
	let serverServerIds = Set.fromList $ entityKey <$> servers
	let allServerIds = serverServerIds
		`Set.union` setFromFieldVal serverWebsitesJoin
		`Set.union` setFromFieldVal serverExtraUsersJoin
		`Set.union` setFromFieldVal serverPoisJoin

	let projectProjectIds = Set.fromList $ entityKey <$> projects
	allServers <- runSqlBackend sqlBackend
		(getByIds ServerId $ Set.toList allServerIds)
        let projectServersJoin = Join serverProjectId allServers
	let serverProjectIds = Set.fromList $ serverProjectId . entityVal <$> allServers
	let allProjectIds = Set.unions [projectProjectIds, serverProjectIds]
	allProjects <- runSqlBackend sqlBackend (getByIds ProjectId $ Set.toList allProjectIds)
	projectRefs <- mapM newObjectDC allProjects

	mapM (getProjectSearchMatch projectServersJoin serverWebsitesJoin serverExtraUsersJoin serverPoisJoin projectPoisJoin) projectRefs