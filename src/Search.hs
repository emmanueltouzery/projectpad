{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, NoMonoLocalBinds, RecordWildCards #-}
module Search where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Database.Esqueleto hiding (on)
import Graphics.QML
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Data.Ord
import Data.Function
import Data.Maybe
import Control.Error

import Model
import Util
import ProjectView

-- I don't know in the QML how to get the current
-- item of the parent repeater => keep in the info
-- at the child level because I need it when some
-- menu actions are activated.
type EntityParentChildInfo a b = ParentChildInfo a (Entity b)

type ServerChildInfo a  = EntityParentChildInfo Server a
type ProjectChildInfo a = EntityParentChildInfo Project a

data ParentChildInfo a b = ParentChildInfo
    {
        pciParent :: EntityRef a,
        pciChild  :: ObjRef b
    } deriving Typeable
instance (Typeable a, Typeable b) => DefaultClass (ParentChildInfo a b) where
    classMembers =
        [
            prop "parent" pciParent,
            prop "child"  pciChild
        ]

data ServerSearchMatch = ServerSearchMatch
    {
        smServer           :: EntityRef Server,
        smServerProject    :: EntityRef Project,
        smServerWebsites   :: [ObjRef (ServerChildInfo ServerWebsite)],
        smServerExtraUsers :: [ObjRef (ServerChildInfo ServerExtraUserAccount)],
        smServerPois       :: [ObjRef (ServerChildInfo ServerPointOfInterest)],
        smServerDatabases  :: [ObjRef (ServerChildInfo ServerDatabase)]
    } deriving Typeable

prop :: (QmlReturnable tr, Typeable b) =>
    String -> (b -> tr) -> Member (GetObjType (ObjRef b))
prop txt cb = defPropertyConst txt (return . cb . fromObjRef)

instance DefaultClass ServerSearchMatch where
    classMembers =
        [
            prop "server" smServer,
            prop "project" smServerProject,
            prop "websites" smServerWebsites,
            prop "extraUsers" smServerExtraUsers,
            prop "pois" smServerPois,
            prop "databases" smServerDatabases
        ]

data ProjectSearchMatch = ProjectSearchMatch
    {
        smProject         :: EntityRef Project,
        smProjectNotes    :: [ObjRef (ProjectChildInfo ProjectNote)],
        smProjectPois     :: [ObjRef (ProjectChildInfo ProjectPointOfInterest)],
        smProjectServers  :: [ObjRef ServerSearchMatch],
        smProjectSrvLinks :: [ObjRef (ParentChildInfo Project ServerLinkInfo)]
    } deriving Typeable

instance DefaultClass ProjectSearchMatch where
    classMembers =
        [
            prop "project"  smProject,
            prop "notes"    smProjectNotes,
            prop "pois"     smProjectPois,
            prop "servers"  smProjectServers,
            prop "srvLinks" smProjectSrvLinks
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

filterProjectNotes :: SqlExpr (Value Text) -> SqlPersistM [Entity ProjectNote]
filterProjectNotes query = select $ from $ \n -> do
    where_ ((n ^. ProjectNoteTitle `like` query)
        ||. (n ^. ProjectNoteContents `like` query))
    return n

filterServerLinks :: SqlExpr (Value Text) -> SqlPersistM [Entity ServerLink]
filterServerLinks query = select $ from $ \n -> do
    where_ (n ^. ServerLinkDesc `like` query)
    return n

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

getByIds :: SqlEntity a =>
    EntityField a (Key a) -> [Key a] -> SqlPersistM [Entity a]
getByIds keySelector ids = select $ from $ \s -> do
    where_ (s ^. keySelector `in_` valList ids)
    return s

-- filter a list of entities from a child table to get only those
-- matching a FK from the parent table and serialize them to ObjRef for QML.
filterEntityJoin :: (DefaultClass (Entity a), Eq (Key b)) =>
    Key b -> Join a b -> IO [EntityRef a]
filterEntityJoin parentKey (Join parentKeyGetter entities) = mapM newObjectDC $
    filter (\e -> parentKeyGetter (entityVal e) == parentKey) entities

makeParentChildInfos :: (DefaultClass (Entity b), Typeable a, Eq (Key a)) =>
    EntityRef a -> Join b a -> IO [ObjRef (EntityParentChildInfo a b)]
makeParentChildInfos parent childrenJoin = do
    let parentKey = entityKey (fromObjRef parent)
    children <- filterEntityJoin parentKey childrenJoin
    mapM (newObjectDC . ParentChildInfo parent) children

data ServerJoins = ServerJoins
     {
         serverWebsitesJoin   :: ServerJoin ServerWebsite,
         serverExtraUsersJoin :: ServerJoin ServerExtraUserAccount,
         serverPoisJoin       :: ServerJoin ServerPointOfInterest,
         serverDatabasesJoin  :: ServerJoin ServerDatabase
     }

getServerSearchMatch :: EntityRef Project -> ServerJoins
    -> EntityRef Server -> IO (ObjRef ServerSearchMatch)
getServerSearchMatch project ServerJoins{..} server =
    newObjectDC =<< ServerSearchMatch server project
        <$> makeParentChildInfos server serverWebsitesJoin
        <*> makeParentChildInfos server serverExtraUsersJoin
        <*> makeParentChildInfos server serverPoisJoin
        <*> makeParentChildInfos server serverDatabasesJoin

getProjectSearchMatch :: SqlBackend -> ProjectJoin Server -> ServerJoins
    -> ProjectJoin ProjectPointOfInterest
    -> ProjectJoin ProjectNote
    -> ProjectJoin ServerLink
    -> EntityRef Project -> IO (ObjRef ProjectSearchMatch)
getProjectSearchMatch sqlBackend projectServersJoin serverJoins
    projectPoisJoin projectNotesJoin projectSrvLinksJoin project = do
    let projectKey = entityKey (fromObjRef project)
    servers <- sortBy compareServerEntities <$> filterEntityJoin projectKey projectServersJoin
    let serverSearchMatch = mapM (getServerSearchMatch project serverJoins) servers
    serverLinkMatches <- makeParentChildInfos project projectSrvLinksJoin
    newObjectDC =<< ProjectSearchMatch project
        <$> makeParentChildInfos project projectNotesJoin
        <*> makeParentChildInfos project projectPoisJoin
        <*> serverSearchMatch
        <*> mapM (srvLinksToSrvLinkInfos sqlBackend . fromObjRef) serverLinkMatches

srvLinksToSrvLinkInfos :: SqlBackend -> ProjectChildInfo ServerLink
                       -> IO (ObjRef (ParentChildInfo Project ServerLinkInfo))
srvLinksToSrvLinkInfos sqlBackend childInfo = do
    serverLinkInfoRefs <- serverLinksGetInfo sqlBackend [fromObjRef $ pciChild childInfo]
    let serverLinkInfoRef = fromMaybe (error "srvLinksInfo returns empty list??") $ headZ serverLinkInfoRefs
    newObjectDC $ ParentChildInfo (pciParent childInfo) serverLinkInfoRef

compareServerEntities :: EntityRef Server -> EntityRef Server -> Ordering
compareServerEntities ea eb = if envCompare /= EQ then envCompare else descCompare
    where
        [a, b] = fromEntityRef <$> [ea, eb]
        envCompare  = (compare `on` serverEnvironment) a b
        descCompare = (compare `on` T.toCaseFold . serverDesc) a b

-- A Join contains a list of child entities and the function
-- from entity to parent key.
data Join a b = Join (a -> Key b) [Entity a]
type ServerJoin a  = Join a Server
type ProjectJoin a = Join a Project

joinGetParentKeys :: Ord (Key b) => Join a b -> Set (Key b)
joinGetParentKeys (Join parentKeyGetter entities) =
    Set.fromList $ parentKeyGetter . entityVal <$> entities

-- that's the entry point for search
searchText :: SqlBackend -> EntityType -> Text -> IO [ObjRef ProjectSearchMatch]
searchText sqlBackend entityType txt = do
    serverWebsitesJoin   <- joinM ServerWebsiteEntityType serverWebsiteServerId filterServerWebsites
    serverExtraUsersJoin <- joinM ServerExtraUserEntityType serverExtraUserAccountServerId filterServerExtraUsers
    serverPoisJoin       <- joinM ServerPoiEntityType serverPointOfInterestServerId filterServerPois
    serverDatabasesJoin  <- joinM DatabaseEntityType serverDatabaseServerId filterServerDatabases

    serverServerIds <- fmap entityKey <$> fetchM ServerEntityType filterServers

    -- start by the leaves of the tree, and go up,
    -- to catch all the cases.
    let allServerIds = Set.unions
                       [
                           Set.fromList serverServerIds,
                           joinGetParentKeys serverWebsitesJoin,
                           joinGetParentKeys serverExtraUsersJoin,
                           joinGetParentKeys serverPoisJoin,
                           joinGetParentKeys serverDatabasesJoin
                       ]
    allServers <- runSqlBackend sqlBackend
                  (getByIds ServerId $ Set.toList allServerIds)
    let serverProjectIds = Set.fromList $ serverProjectId . entityVal <$> allServers
    let projectServersJoin = Join serverProjectId allServers

    projectPoisJoin  <- joinM ProjectPoiEntityType projectPointOfInterestProjectId filterProjectPois
    projectNotesJoin <- joinM ProjectNoteEntityType projectNoteProjectId filterProjectNotes
    projectSrvLinksJoin <- joinM ServerLinkEntityType serverLinkProjectId filterServerLinks

    projectProjectIds <- fmap entityKey <$> fetchM ProjectEntityType filterProjects

    let allProjectIds = Set.unions
                        [
                            serverProjectIds,
                            Set.fromList projectProjectIds,
                            joinGetParentKeys projectPoisJoin,
                            joinGetParentKeys projectNotesJoin,
                            joinGetParentKeys projectSrvLinksJoin
                        ]
    allProjects <- runSqlBackend sqlBackend (getByIds ProjectId $ Set.toList allProjectIds)
    projectRefs <- mapM newObjectDC
        $ sortBy (comparing $ T.toCaseFold . projectName . entityVal) allProjects
    let serverJoins = ServerJoins serverWebsitesJoin
          serverExtraUsersJoin serverPoisJoin serverDatabasesJoin
    mapM (getProjectSearchMatch sqlBackend projectServersJoin serverJoins
          projectPoisJoin projectNotesJoin projectSrvLinksJoin) projectRefs
    where
        query = (%) ++. val txt ++. (%)
        runQ f = runSqlBackend sqlBackend (f query)
        fetchM e g = if e == entityType || entityType == AllEntityTypes
                     then runQ g
                     else return []
        joinM e f g = Join f <$> fetchM e g
