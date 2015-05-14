{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, TypeFamilies #-}
module Search where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Database.Esqueleto hiding (on)
import Graphics.QML
import Control.Applicative
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Data.Ord
import Data.Function

import Model

-- I don't know in the QML how to get the current
-- item of the parent repeater => keep in the info
-- at the child level because I need it when some
-- menu actions are activated.
data ServerChildInfo a = ServerChildInfo
    {
        sciServer :: ObjRef (Entity Server),
        sciChild  :: ObjRef (Entity a)
    } deriving Typeable

instance Typeable a => DefaultClass (ServerChildInfo a) where
    classMembers =
        [
            prop "server" sciServer,
            prop "child"  sciChild
        ]

data ServerSearchMatch = ServerSearchMatch
    {
        smServer           :: ObjRef (Entity Server),
        smServerWebsites   :: [ObjRef (ServerChildInfo ServerWebsite)],
        smServerExtraUsers :: [ObjRef (ServerChildInfo ServerExtraUserAccount)],
        smServerPois       :: [ObjRef (ServerChildInfo ServerPointOfInterest)],
        smServerDatabases  :: [ObjRef (ServerChildInfo ServerDatabase)]
    } deriving Typeable

prop :: (Marshal tr, Typeable b, MarshalMode tr ICanReturnTo () ~ Yes) =>
    String -> (b -> tr) -> Member (GetObjType (ObjRef b))
prop txt cb = defPropertyConst txt (return . cb . fromObjRef)

instance DefaultClass ServerSearchMatch where
    classMembers =
        [
            prop "server" smServer,
            prop "websites" smServerWebsites,
            prop "extraUsers" smServerExtraUsers,
            prop "pois" smServerPois,
            prop "databases" smServerDatabases
        ]

data ProjectSearchMatch = ProjectSearchMatch
    {
        smProject        :: ObjRef (Entity Project),
        smProjectPois    :: [ObjRef (Entity ProjectPointOfInterest)],
        smProjectServers :: [ObjRef ServerSearchMatch]
    } deriving Typeable

instance DefaultClass ProjectSearchMatch where
    classMembers =
        [
            prop "project" smProject,
            prop "pois" smProjectPois,
            prop "servers" smProjectServers
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

filterServerDatabases :: SqlExpr (Value Text) -> SqlPersistM [Entity ServerDatabase]
filterServerDatabases query = select $ from $ \d -> do
    where_ ((d ^. ServerDatabaseDesc `like` query)
        ||. (d ^. ServerDatabaseName `like` query)
        ||. (d ^. ServerDatabaseText `like` query))
    return d

getByIds :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a) =>
    EntityField a (Key a) -> [Key a] -> SqlPersistM [Entity a]
getByIds keySelector ids = select $ from $ \s -> do
    where_ (s ^. keySelector `in_` valList ids)
    return s

-- filter a list of entities from a child table to get only those
-- matching a FK from the parent table and serialize them to ObjRef for QML.
filterEntityJoin :: (DefaultClass (Entity a), Eq (Key b)) =>
    Key b -> Join a b -> IO [ObjRef (Entity a)]
filterEntityJoin parentKey (Join parentKeyGetter entities) = mapM newObjectDC $
    filter (\e -> parentKeyGetter (entityVal e) == parentKey) entities

makeServerChildInfos :: (DefaultClass (Entity a), Typeable a) =>
    ObjRef (Entity Server)
    -> Join a Server -> IO [ObjRef (ServerChildInfo a)]
makeServerChildInfos server childrenJoin = do
    let serverKey = entityKey (fromObjRef server)
    children <- filterEntityJoin serverKey childrenJoin
    mapM (newObjectDC . ServerChildInfo server) children

getServerSearchMatch :: ServerJoin ServerWebsite -> ServerJoin ServerExtraUserAccount -> ServerJoin ServerPointOfInterest
    -> ServerJoin ServerDatabase -> ObjRef (Entity Server) -> IO (ObjRef ServerSearchMatch)
getServerSearchMatch serverWebsitesJoin serverExtraUsersJoin serverPoisJoin serverDatabasesJoin server =
    newObjectDC =<< ServerSearchMatch server
        <$> makeServerChildInfos server serverWebsitesJoin
        <*> makeServerChildInfos server serverExtraUsersJoin
        <*> makeServerChildInfos server serverPoisJoin
        <*> makeServerChildInfos server serverDatabasesJoin

getProjectSearchMatch :: ProjectJoin Server -> ServerJoin ServerWebsite -> ServerJoin ServerExtraUserAccount
    -> ServerJoin ServerPointOfInterest -> ServerJoin ServerDatabase -> ProjectJoin ProjectPointOfInterest
    -> ObjRef (Entity Project) -> IO (ObjRef ProjectSearchMatch)
getProjectSearchMatch projectServersJoin serverWebsitesJoin serverExtraUsersJoin serverPoisJoin serverDatabasesJoin projectPoisJoin project = do
    let projectKey = entityKey (fromObjRef project)
    servers <- sortBy compareServerEntities <$> filterEntityJoin projectKey projectServersJoin
    let serverSearchMatch = mapM (getServerSearchMatch serverWebsitesJoin serverExtraUsersJoin
                                  serverPoisJoin serverDatabasesJoin) servers
    newObjectDC =<< ProjectSearchMatch project
        <$> filterEntityJoin projectKey projectPoisJoin
        <*> serverSearchMatch

compareServerEntities :: ObjRef (Entity Server) -> ObjRef (Entity Server) -> Ordering
compareServerEntities ea eb = if envCompare /= EQ then envCompare else descCompare
    where
        [a, b] = entityVal . fromObjRef <$> [ea, eb]
        envCompare = (compare `on` serverEnvironment) a b
        descCompare = (compare `on` T.toCaseFold . serverDesc) a b

-- A Join contains a list of child entities and the function
-- from entity to parent key.
data Join a b = Join (a -> Key b) [Entity a]
type ServerJoin a = Join a Server
type ProjectJoin a = Join a Project

joinGetParentKeys :: Ord (Key b) => Join a b -> Set (Key b)
joinGetParentKeys (Join parentKeyGetter entities) = Set.fromList $ parentKeyGetter . entityVal <$> entities

searchText :: SqlBackend -> Text -> IO [ObjRef ProjectSearchMatch]
searchText sqlBackend txt = do
    projectPoisJoin      <- Join projectPointOfInterestProjectId <$> runQ filterProjectPois
    serverWebsitesJoin   <- Join serverWebsiteServerId <$> runQ filterServerWebsites
    serverExtraUsersJoin <- Join serverExtraUserAccountServerId <$> runQ filterServerExtraUsers
    serverPoisJoin       <- Join serverPointOfInterestServerId <$> runQ filterServerPois
    serverDatabasesJoin  <- Join serverDatabaseServerId <$> runQ filterServerDatabases
    servers  <- runQ filterServers
    projects <- runQ filterProjects

    -- start by the leaves of the tree, and go up,
    -- to catch all the cases.
    let serverServerIds = Set.fromList $ entityKey <$> servers
    let allServerIds = serverServerIds
                       `Set.union` joinGetParentKeys serverWebsitesJoin
                       `Set.union` joinGetParentKeys serverExtraUsersJoin
                       `Set.union` joinGetParentKeys serverPoisJoin
                       `Set.union` joinGetParentKeys serverDatabasesJoin

    let projectProjectIds = Set.fromList $ entityKey <$> projects
    allServers <- runSqlBackend sqlBackend
                  (getByIds ServerId $ Set.toList allServerIds)
    let projectServersJoin = Join serverProjectId allServers
    let serverProjectIds = Set.fromList $ serverProjectId . entityVal <$> allServers
    let allProjectIds = Set.unions [projectProjectIds, serverProjectIds, joinGetParentKeys projectPoisJoin]
    allProjects <- runSqlBackend sqlBackend (getByIds ProjectId $ Set.toList allProjectIds)
    projectRefs <- mapM newObjectDC
        $ sortBy (comparing $ T.toCaseFold . projectName . entityVal) allProjects
    mapM (getProjectSearchMatch projectServersJoin serverWebsitesJoin serverExtraUsersJoin
        serverPoisJoin serverDatabasesJoin projectPoisJoin) projectRefs
    where
        query = (%) ++. val txt ++. (%)
        runQ :: (SqlExpr (Value Text) -> SqlPersistM [Entity a]) -> IO [Entity a]
        runQ f = runSqlBackend sqlBackend (f query)
