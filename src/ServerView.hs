{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, ViewPatterns #-}
module ServerView where

import Control.Applicative
import Graphics.QML
import Graphics.QML.Objects.ParamNames
import Database.Esqueleto
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Persist as P
import Data.List
import Data.Maybe
import Data.Ord

import Model
import ModelBase
import CrudHelpers
import System
import Util

type ServerViewState = ()

readServerPois :: Int -> SqlPersistM [Entity ServerPointOfInterest]
readServerPois serverId = select $ from $ \p -> do
    where_ (p ^. ServerPointOfInterestServerId ==. val (toSqlKey32 serverId))
    orderBy [asc (p ^. ServerPointOfInterestDesc)]
    return p

-- I have decided in the schema to have null for "no group".
-- In that sense, "" makes no sense, because it would also
-- mean "no group"... So make sure we never send "" to the DB.
-- I also have a constraint there, length>0.
groupOrNothing :: Maybe Text -> Maybe Text
groupOrNothing (Just "") = Nothing
groupOrNothing x@_ = x

addServerPoi :: SqlBackend -> Int
    -> Text -> Text -> Text -> Text -> Maybe Text -> IO ()
addServerPoi sqlBackend serverId pDesc path txt interestTypeT (groupOrNothing -> grpName) = do
    let interestType = read $ T.unpack interestTypeT
    addHelper sqlBackend serverId
        $ ServerPointOfInterest pDesc path txt interestType grpName

updateServerPoi :: SqlBackend -> ObjRef (Entity ServerPointOfInterest)
    -> Text -> Text -> Text -> Text -> Maybe Text -> IO (ObjRef (Entity ServerPointOfInterest))
updateServerPoi sqlBackend poiRef
    pDesc path txt interestTypeT (groupOrNothing -> grpName) = do
    let interestType = read $ T.unpack interestTypeT
    updateHelper sqlBackend poiRef
        [
            ServerPointOfInterestDesc P.=. pDesc, ServerPointOfInterestPath P.=. path,
            ServerPointOfInterestText P.=. txt,
            ServerPointOfInterestInterestType P.=. interestType,
            ServerPointOfInterestGroupName P.=. grpName
        ]

readServerWebsites :: Int -> SqlPersistM [Entity ServerWebsite]
readServerWebsites serverId = select $ from $ \p -> do
    where_ (p ^. ServerWebsiteServerId ==. val (toSqlKey32 serverId))
    orderBy [asc (p ^. ServerWebsiteDesc)]
    return p

addServerWebsite :: SqlBackend -> Int
    -> Text -> Text -> Text -> Text -> Text -> Maybe Int -> Maybe Text -> IO ()
addServerWebsite sqlBackend serverId
    pDesc url txt username password mDatabaseId (groupOrNothing -> grpName) = do
    let mDatabaseKey = toSqlKey32 <$> mDatabaseId
    addHelper sqlBackend serverId
        $ ServerWebsite pDesc url txt username password mDatabaseKey grpName

updateServerWebsite :: SqlBackend -> ObjRef (Entity ServerWebsite)
    -> Text -> Text -> Text -> Text -> Text
    -> Maybe Int -> Maybe Text -> IO (ObjRef (Entity ServerWebsite))
updateServerWebsite sqlBackend srvWwwRef
    pDesc url txt username password mDatabaseId (groupOrNothing -> grpName) = do
    let mDatabaseKey = toSqlKey32 <$> mDatabaseId
    updateHelper sqlBackend srvWwwRef
        [
            ServerWebsiteDesc P.=. pDesc, ServerWebsiteUrl P.=. url,
            ServerWebsiteText P.=. txt, ServerWebsiteUsername P.=. username,
            ServerWebsitePassword P.=. password,
            ServerWebsiteServerDatabaseId P.=. mDatabaseKey,
            ServerWebsiteGroupName P.=. grpName
        ]

readServerDatabases :: Int -> SqlPersistM [Entity ServerDatabase]
readServerDatabases serverId = select $ from $ \p -> do
    where_ (p ^. ServerDatabaseServerId ==. val (toSqlKey32 serverId))
    orderBy [asc (p ^. ServerDatabaseDesc)]
    return p

addServerDatabase :: SqlBackend -> Int
    -> Text -> Text -> Text -> Text -> Text -> Maybe Text -> IO ()
addServerDatabase sqlBackend serverId pDesc name txt
    username password (groupOrNothing -> grpName) = addHelper sqlBackend serverId
        $ ServerDatabase pDesc name txt username password grpName

updateServerDatabase :: SqlBackend -> ObjRef (Entity ServerDatabase)
    -> Text -> Text -> Text -> Text -> Text -> Maybe Text -> IO (ObjRef (Entity ServerDatabase))
updateServerDatabase sqlBackend srvDbRef
    pDesc name txt username password (groupOrNothing -> grpName) = updateHelper sqlBackend srvDbRef
        [
            ServerDatabaseDesc P.=. pDesc, ServerDatabaseName P.=. name,
            ServerDatabaseText P.=. txt,
            ServerDatabaseUsername P.=. username,
            ServerDatabasePassword P.=. password,
            ServerDatabaseGroupName P.=. grpName
        ]

canDeleteServerDatabase :: SqlBackend -> (Key Server -> Bool) -> Entity ServerDatabase -> IO (Maybe Text)
canDeleteServerDatabase sqlBackend serverFilter serverDb = do
    websites <- fmap entityVal <$> runSqlBackend sqlBackend
        (select $ from $ \w -> do
        where_ (w ^. ServerWebsiteServerDatabaseId ==. val (Just $ entityKey serverDb))
        return w)
    if not (any (serverFilter . serverWebsiteServerId) websites)
        then return Nothing
        else do
            let serverList = T.intercalate ", " $ serverWebsiteDesc <$> websites
            let name = serverDatabaseName $ entityVal serverDb
            let strElts = ["Can't delete ", name, ": it's used by servers ",  serverList]
            return $ Just $ T.concat strElts

getAllDatabases :: SqlBackend -> IO [ObjRef (Entity ServerDatabase)]
getAllDatabases sqlBackend = do
    dbs <- runSqlBackend sqlBackend (select $ from $ \p -> do
        orderBy [asc (p ^. ServerDatabaseDesc)]
        return p)
    mapM newObjectDC dbs

readServerExtraUserAccounts :: Int -> SqlPersistM [Entity ServerExtraUserAccount]
readServerExtraUserAccounts serverId = select $ from $ \p -> do
    where_ (p ^. ServerExtraUserAccountServerId ==. val (toSqlKey32 serverId))
    orderBy [asc (p ^. ServerExtraUserAccountDesc)]
    return p

addServerExtraUserAccount :: SqlBackend -> Int
    -> Text -> Text -> Text -> Text -> Maybe Text -> IO ()
addServerExtraUserAccount sqlBackend serverId
    pDesc username password keyPath (groupOrNothing -> grpName) = do
    authKeyInfo <- processAuthKeyInfo keyPath
    addHelper sqlBackend serverId $ ServerExtraUserAccount username password pDesc
            (fst <$> authKeyInfo) (snd <$> authKeyInfo) grpName

updateServerExtraUserAccount :: SqlBackend -> ObjRef (Entity ServerExtraUserAccount)
    -> Text -> Text -> Text -> Text -> Maybe Text -> IO (ObjRef (Entity ServerExtraUserAccount))
updateServerExtraUserAccount sqlBackend acctRef
    pDesc username password keyPath (groupOrNothing -> grpName) = do
    authKeyInfo <- processAuthKeyInfo keyPath
    updateHelper sqlBackend acctRef
        [
            ServerExtraUserAccountDesc P.=. pDesc, ServerExtraUserAccountUsername P.=. username,
            ServerExtraUserAccountPassword P.=. password,
            ServerExtraUserAccountAuthKey P.=. fst <$> authKeyInfo,
            ServerExtraUserAccountAuthKeyFilename P.=. snd <$> authKeyInfo,
            ServerExtraUserAccountGroupName P.=. grpName
        ]

saveExtraUserAuthKey :: Text -> ObjRef (Entity ServerExtraUserAccount) -> IO (Either Text Text)
saveExtraUserAuthKey path (entityVal . fromObjRef -> userAcct) = saveAuthKeyBytes path $
    serverExtraUserAccountAuthKey userAcct

executePoiAction :: ObjRef ServerViewState -> ObjRef (Entity Server)
    -> ObjRef (Entity ServerPointOfInterest) -> IO (Either Text ())
executePoiAction srvState (entityVal . fromObjRef -> server)
        (entityVal . fromObjRef -> serverPoi) =
    case serverPointOfInterestInterestType serverPoi of
        PoiCommandToRun -> executePoiCommand srvState server serverPoi
        PoiLogFile      -> executePoiLogFile server serverPoi "tail -f "
        PoiConfigFile   -> executePoiLogFile server serverPoi "vim "
        _               -> return $ Right ()

executePoiSecondaryAction :: ObjRef (Entity Server)
    -> ObjRef (Entity ServerPointOfInterest) -> IO (Either Text ())
executePoiSecondaryAction (entityVal . fromObjRef -> server)
        (entityVal . fromObjRef -> serverPoi) =
    case serverPointOfInterestInterestType serverPoi of
        PoiLogFile -> executePoiLogFile server serverPoi "less "
        _ -> return $ Right ()

executePoiLogFile :: Server -> ServerPointOfInterest -> Text -> IO (Either Text ())
executePoiLogFile server serverPoi cmd = openSshSession (serverIp server) (serverUsername server)
    (serverPassword server) (Just $ cmd `T.append` serverPointOfInterestPath serverPoi)

executePoiCommand :: ObjRef ServerViewState -> Server -> ServerPointOfInterest -> IO (Either Text ())
executePoiCommand srvState server serverPoi = do
    let workDir = case serverPointOfInterestPath serverPoi of
          "" -> Nothing
          x  -> Just x
    Right <$> runProgramOverSshAsync (serverIp server)
        (serverUsername server) (serverPassword server)
        workDir (serverPointOfInterestText serverPoi)
        (fireSignal (Proxy :: Proxy SignalOutput) srvState . cmdProgressToJs)

deleteServerPoi :: Key ServerPointOfInterest -> SqlPersistM ()
deleteServerPoi = P.delete

deleteServerWebsite :: Key ServerWebsite -> SqlPersistM ()
deleteServerWebsite = P.delete

deleteServerDatabase :: Key ServerDatabase -> SqlPersistM ()
deleteServerDatabase = P.delete

deleteServerExtraUserAccount :: Key ServerExtraUserAccount -> SqlPersistM ()
deleteServerExtraUserAccount = P.delete

getServerGroupNames :: SqlBackend -> Int -> IO [Text]
getServerGroupNames sqlBackend serverId = do
    poiGroupNames <- readEF serverPointOfInterestGroupName readServerPois
    wwwGroupNames <- readEF serverWebsiteGroupName readServerWebsites
    dbGroupNames  <- readEF serverDatabaseGroupName readServerDatabases
    extraAccountGroupNames <- readEF serverExtraUserAccountGroupName readServerExtraUserAccounts
    return $ nub $ sortBy (comparing T.toCaseFold) $ catMaybes $
        poiGroupNames ++ wwwGroupNames ++ dbGroupNames ++ extraAccountGroupNames
    where
      readEF :: (a -> b) -> (Int -> SqlPersistM [Entity a]) -> IO [b]
      readEF f r = fmap (f . entityVal) <$> runSqlBackend sqlBackend (r serverId)

data ServerDisplaySection = ServerDisplaySection
    {
        srvSectionGrpName    :: Maybe Text,
        srvSectionPois       :: [ObjRef (Entity ServerPointOfInterest)],
        srvSectionWebsites   :: [ObjRef (Entity ServerWebsite)],
        srvSectionDatabases  :: [ObjRef (Entity ServerDatabase)],
        srvSectionExtraUsers :: [ObjRef (Entity ServerExtraUserAccount)]
    } deriving Typeable

instance DefaultClass ServerDisplaySection where
    classMembers =
        [
            defPropertyConst "groupName"  (readM srvSectionGrpName),
            defPropertyConst "pois"       (readM srvSectionPois),
            defPropertyConst "websites"   (readM srvSectionWebsites),
            defPropertyConst "databases"  (readM srvSectionDatabases),
            defPropertyConst "extraUsers" (readM srvSectionExtraUsers)
        ]

getServerDisplaySections :: SqlBackend -> Int -> IO [ObjRef ServerDisplaySection]
getServerDisplaySections sqlBackend serverId = do
    groupNames <- getServerGroupNames sqlBackend serverId
    pois       <- runServerQ readServerPois
    websites   <- runServerQ readServerWebsites
    databases  <- runServerQ readServerDatabases
    extraUsers <- runServerQ readServerExtraUserAccounts
    let sectionForGroup grp = ServerDisplaySection {
            srvSectionGrpName    = grp,
            srvSectionPois       = filterForGroup grp serverPointOfInterestGroupName pois,
            srvSectionWebsites   = filterForGroup grp serverWebsiteGroupName websites,
            srvSectionDatabases  = filterForGroup grp serverDatabaseGroupName databases,
            srvSectionExtraUsers = filterForGroup grp serverExtraUserAccountGroupName extraUsers
        }
    mapM newObjectDC $ sectionForGroup Nothing : (sectionForGroup . Just <$> groupNames)
    where
      runServerQ :: DefaultClass a => (Int -> SqlPersistM [a]) -> IO [ObjRef a]
      runServerQ f = sqlToQml sqlBackend (f serverId)
      filterForGroup grp grpNameField = filter ((==grp) . grpNameField . entityVal . fromObjRef)

createServerViewState :: SqlBackend -> IO (ObjRef ServerViewState)
createServerViewState sqlBackend = do
    serverViewClass <- newClass
        [
            defStatic  "getServerDisplaySections" (getServerDisplaySections sqlBackend),
            defStatic  "addServerPoi"             (addServerPoi sqlBackend),
            defStatic  "updateServerPoi"          (updateServerPoi sqlBackend),
            defMethod' "deleteServerPois"         (deleteHelper sqlBackend deleteServerPoi),
            defStatic  "addServerWebsite"         (addServerWebsite sqlBackend),
            defStatic  "updateServerWebsite"      (updateServerWebsite sqlBackend),
            defMethod' "deleteServerWebsites"     (deleteHelper sqlBackend deleteServerWebsite),
            defStatic  "addServerDatabase"        (addServerDatabase sqlBackend),
            defStatic  "updateServerDatabase"     (updateServerDatabase sqlBackend),
            defStatic  "canDeleteServerDatabase"  (canDeleteServerDatabase sqlBackend (const True) . fromObjRef),
            defMethod' "deleteServerDatabases"    (deleteHelper sqlBackend deleteServerDatabase),
            defStatic  "getAllDatabases"          (getAllDatabases sqlBackend),
            defStatic  "addServerExtraUserAccount" (addServerExtraUserAccount sqlBackend),
            defStatic  "updateServerExtraUserAccount" (updateServerExtraUserAccount sqlBackend),
            defMethod' "deleteServerExtraUserAccounts" (deleteHelper sqlBackend deleteServerExtraUserAccount),
            defStatic  "getServerGroupNames"      (getServerGroupNames sqlBackend),
            defStatic  "saveAuthKey"              (serializeEitherM . saveExtraUserAuthKey),
            defMethod' "executePoiAction"         (\srvState server serverPoi -> serializeEither' <$>
                                                      executePoiAction srvState server serverPoi),
            defStatic "executePoiSecondaryAction" (serializeEitherM' . executePoiSecondaryAction),
            defSignalNamedParams "gotOutput"      (Proxy :: Proxy SignalOutput) $ fstName "output"
        ]
    newObject serverViewClass ()
