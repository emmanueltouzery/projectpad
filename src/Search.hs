{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, DeriveDataTypeable, ExistentialQuantification, RankNTypes #-}
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

filterParentEntity :: Eq a => a -> (record -> a) -> [Entity record] -> [Entity record]
filterParentEntity parentKey fieldGetter = filter (\e -> fieldGetter (entityVal e) == parentKey)

filterEntityJoin :: Key Server -> ServerJoin record -> [Entity record]
filterEntityJoin parentKey (ServerJoin fieldGetter entities) = filterParentEntity parentKey fieldGetter entities

filterForServerDC serverKey = mapM newObjectDC . filterEntityJoin serverKey

getServerSearchMatch :: ServerJoin ServerWebsite -> ServerJoin ServerExtraUserAccount -> ServerJoin ServerPointOfInterest
                     -> ObjRef (Entity Server) -> IO (ObjRef ServerSearchMatch)
getServerSearchMatch serverWebsitesJoin serverExtraUsersJoin serverPoisJoin  server = do
	let serverKey = entityKey (fromObjRef server)
	newObjectDC =<< ServerSearchMatch server
		<$> filterForServerDC serverKey serverWebsitesJoin
		<*> filterForServerDC serverKey serverExtraUsersJoin
		<*> filterForServerDC serverKey serverPoisJoin

getProjectSearchMatch :: [Entity Server] -> ServerJoin ServerWebsite -> ServerJoin ServerExtraUserAccount
	-> ServerJoin ServerPointOfInterest -> [Entity ProjectPointOfInterest]
	-> ObjRef (Entity Project) -> IO (ObjRef ProjectSearchMatch)
getProjectSearchMatch allServers serverWebsitesJoin serverExtraUsersJoin serverPoisJoin allProjectPois project = do
	let projectKey = entityKey (fromObjRef project)
	let filterForProject = filterParentEntity projectKey
	let projectServers = filterForProject serverProjectId allServers
	serverSearchMatch <- mapM (getServerSearchMatch serverWebsitesJoin serverExtraUsersJoin serverPoisJoin )
		<$> mapM newObjectDC projectServers
	let projectPois = filterForProject projectPointOfInterestProjectId allProjectPois
	newObjectDC =<< ProjectSearchMatch project
		<$> mapM newObjectDC projectPois
		<*> serverSearchMatch

data ServerJoin a = ServerJoin (a -> Key Server) [Entity a]

setFromFieldVal :: ServerJoin a -> Set.Set (Key Server)
setFromFieldVal (ServerJoin fieldGetter entities) = Set.fromList $ fieldGetter . entityVal <$> entities

searchText :: SqlBackend -> Text -> IO [ObjRef ProjectSearchMatch]
searchText sqlBackend txt = do
	let query = (%) ++. val txt ++. (%)
	let runQ f = runSqlBackend sqlBackend (f query)
	projects <- runQ filterProjects
	allProjectPois <- runQ filterProjectPois
	servers <- runQ filterServers
	serverWebsitesJoin <- ServerJoin serverWebsiteServerId <$> runQ filterServerWebsites
	serverExtraUsersJoin <- ServerJoin serverExtraUserAccountServerId <$> runQ filterServerExtraUsers
	serverPoisJoin <- ServerJoin serverPointOfInterestServerId <$> runQ filterServerPois

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
	let serverProjectIds = Set.fromList $ serverProjectId . entityVal <$> allServers
	let allProjectIds = Set.unions [projectProjectIds, serverProjectIds]
	allProjects <- runSqlBackend sqlBackend (getByIds ProjectId $ Set.toList allProjectIds)
	projectRefs <- mapM newObjectDC allProjects

	mapM (getProjectSearchMatch allServers serverWebsitesJoin serverExtraUsersJoin serverPoisJoin allProjectPois) projectRefs
