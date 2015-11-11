{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies,
    MultiParamTypeClasses, ViewPatterns, RecordWildCards, NoMonoLocalBinds #-}
module ServerView where

import Control.Applicative
import Graphics.QML
import Graphics.QML.Objects.ParamNames
import Database.Esqueleto
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Persist as P
import Control.Error
import Data.Monoid

import Model
import ModelBase
import CrudHelpers
import System
import Util

serverToSystemServer :: Server -> System.ServerInfo
serverToSystemServer Server{..} = ServerInfo {
        srvAddress  = serverIp,
        srvUsername = serverUsername,
        srvPassword = serverPassword
    }

type ServerViewState = ()

readServerPois :: Int -> SqlPersistM [Entity ServerPointOfInterest]
readServerPois serverId = select $ from $ \p -> do
    where_ (p ^. ServerPointOfInterestServerId ==. val (toSqlKey32 serverId))
    orderBy [asc (p ^. ServerPointOfInterestDesc)]
    return p

addServerPoi :: SqlBackend -> Int
    -> Text -> Text -> Text -> Text -> Maybe Text -> IO ()
addServerPoi sqlBackend serverId pDesc path txt interestTypeT (groupOrNothing -> grpName) = do
    let interestType = read $ T.unpack interestTypeT
    addHelper sqlBackend serverId
        $ ServerPointOfInterest pDesc path txt interestType grpName

updateServerPoi :: SqlBackend -> EntityRef ServerPointOfInterest
    -> Text -> Text -> Text -> Text -> Maybe Text -> IO (EntityRef ServerPointOfInterest)
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

updateServerWebsite :: SqlBackend -> EntityRef ServerWebsite
    -> Text -> Text -> Text -> Text -> Text
    -> Maybe Int -> Maybe Text -> IO (EntityRef ServerWebsite)
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

updateServerDatabase :: SqlBackend -> EntityRef ServerDatabase
    -> Text -> Text -> Text -> Text -> Text -> Maybe Text -> IO (EntityRef ServerDatabase)
updateServerDatabase sqlBackend srvDbRef
    pDesc name txt username password (groupOrNothing -> grpName) = updateHelper sqlBackend srvDbRef
        [
            ServerDatabaseDesc P.=. pDesc, ServerDatabaseName P.=. name,
            ServerDatabaseText P.=. txt,
            ServerDatabaseUsername P.=. username,
            ServerDatabasePassword P.=. password,
            ServerDatabaseGroupName P.=. grpName
        ]

canDeleteServerDatabase :: SqlBackend -> (Key Server -> Bool)
                        -> Entity ServerDatabase -> IO (Maybe Text)
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

updateServerExtraUserAccount :: SqlBackend -> EntityRef ServerExtraUserAccount
    -> Text -> Text -> Text -> Text -> Maybe Text -> IO (EntityRef ServerExtraUserAccount)
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

saveExtraUserAuthKey :: Text -> EntityRef ServerExtraUserAccount -> IO (Either Text Text)
saveExtraUserAuthKey path (entityVal . fromObjRef -> userAcct) =
    saveAuthKeyBytes path (serverExtraUserAccountAuthKey userAcct)

executePoiAction :: SqlBackend -> ObjRef ServerViewState -> EntityRef Server
    -> EntityRef ServerPointOfInterest -> IO (Either Text ())
executePoiAction sqlBackend srvState server (entityVal . fromObjRef -> serverPoi) =
    openServerSshAction sqlBackend server $ \port eSrv ->
    case serverPointOfInterestInterestType serverPoi of
        PoiCommandToRun -> executePoiCommand srvState eSrv port serverPoi
        PoiLogFile      -> executePoiLogFile eSrv port serverPoi "tail -f "
        PoiConfigFile   -> executePoiLogFile eSrv port serverPoi "vim "
        _               -> return $ Right ()

executePoiSecondaryAction :: SqlBackend -> EntityRef Server
    -> EntityRef ServerPointOfInterest -> IO (Either Text ())
executePoiSecondaryAction sqlBackend server (entityVal . fromObjRef -> serverPoi) =
    openServerSshAction sqlBackend server $ \port eSrv ->
    case serverPointOfInterestInterestType serverPoi of
        PoiLogFile -> executePoiLogFile eSrv port serverPoi "less "
        _ -> return $ Right ()

executePoiThirdAction :: SqlBackend -> ObjRef ServerViewState -> EntityRef Server
    -> EntityRef ServerPointOfInterest -> IO (Either Text ())
executePoiThirdAction sqlBackend srvState server (entityVal . fromObjRef -> serverPoi) =
    openServerSshAction sqlBackend server $ \port eSrv ->
    case serverPointOfInterestInterestType serverPoi of
        PoiLogFile -> downloadFileSsh eSrv port (serverPointOfInterestPath serverPoi)
                      (fireSignal (Proxy :: Proxy SignalOutput) srvState . cmdProgressToJs) >> return (Right ())
        _ -> return $ Right ()

executePoiLogFile :: ServerInfo -> Int -> ServerPointOfInterest -> Text -> IO (Either Text ())
executePoiLogFile eSrv port serverPoi cmd = openSshSession eSrv port
    (SshCommand $ cmd `T.append` serverPointOfInterestPath serverPoi)

executePoiCommand :: ObjRef ServerViewState -> ServerInfo -> Int -> ServerPointOfInterest -> IO (Either Text ())
executePoiCommand srvState eSrv port serverPoi = do
    let workDir = case serverPointOfInterestPath serverPoi of
          "" -> Nothing
          x  -> Just x
    Right <$> runProgramOverSshAsync eSrv port
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
    poiGroupNames <- readEF readServerPois serverPointOfInterestGroupName
    wwwGroupNames <- readEF readServerWebsites serverWebsiteGroupName
    dbGroupNames  <- readEF readServerDatabases serverDatabaseGroupName
    extraAccountGroupNames <- readEF readServerExtraUserAccounts serverExtraUserAccountGroupName
    return $ mergeNames $
        poiGroupNames ++ wwwGroupNames ++ dbGroupNames ++ extraAccountGroupNames
    where
      readEF f = readEntityFields sqlBackend (f serverId)

data ServerDisplaySection = ServerDisplaySection
    {
        srvSectionGrpName    :: Maybe Text,
        srvSectionPois       :: [EntityRef ServerPointOfInterest],
        srvSectionWebsites   :: [EntityRef ServerWebsite],
        srvSectionDatabases  :: [EntityRef ServerDatabase],
        srvSectionExtraUsers :: [EntityRef ServerExtraUserAccount]
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

openServerSshAction :: SqlBackend -> EntityRef Server
                     -> (Int -> ServerInfo -> IO (Either Text a))
                     -> IO (Either Text a)
openServerSshAction sqlBackend (entityVal . fromObjRef -> server) callback =
    case serverAccessType server of
      SrvAccessSsh       -> callback sshDefaultPort (serverToSystemServer server)
      SrvAccessSshTunnel -> openServerSshTunnelAction sqlBackend server callback
      _                  -> error $ "called openServerSshAction with "
                                  <> show (serverAccessType server)

openServerSshTunnelAction :: SqlBackend -> Server
                          -> (Int -> ServerInfo -> IO (Either Text a))
                          -> IO (Either Text a)
openServerSshTunnelAction sqlBackend server callback = runExceptT $ do
    tunnelPort     <- noteET "no tunnel port configured" $ serverSshTunnelPort server
    intermediateId <- noteET "no intermediate server configured" $ serverSshTunnelThroughServerId server
    mIntermediate  <- tryET $ runSqlBackend sqlBackend (get intermediateId)
    intermediate   <- noteET "intermediate server missing from DB" mIntermediate
    r <- tryET $ openSshTunnelSession tunnelPort
         (serverToSystemServer intermediate)
         (serverToSystemServer server) callback
    hoistEither r

isSshHostTrusted :: SqlBackend -> EntityRef Server -> IO (Either Text Bool)
isSshHostTrusted sqlBackend server =
    openServerSshAction sqlBackend server isHostTrusted

addInSshHostTrustStore :: SqlBackend -> EntityRef Server -> IO (Either Text ())
addInSshHostTrustStore sqlBackend server =
    openServerSshAction sqlBackend server addInHostTrustStore

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
            defStatic  "addServerExtraUserAccount" (addServerExtraUserAccount sqlBackend),
            defStatic  "updateServerExtraUserAccount" (updateServerExtraUserAccount sqlBackend),
            defMethod' "deleteServerExtraUserAccounts" (deleteHelper sqlBackend deleteServerExtraUserAccount),
            defStatic  "getServerGroupNames"      (getServerGroupNames sqlBackend),
            defStatic  "saveAuthKey"              (liftQmlResult2 saveExtraUserAuthKey),
            defStatic  "isHostTrusted"            (liftQmlResult1 $ isSshHostTrusted sqlBackend),
            defStatic  "addInHostTrustStore"      (liftQmlResult1 $ addInSshHostTrustStore sqlBackend),
            defMethod' "executePoiAction"         (liftQmlResult3 $ executePoiAction sqlBackend),
            defStatic "executePoiSecondaryAction" (liftQmlResult2 $ executePoiSecondaryAction sqlBackend),
            defMethod' "executePoiThirdAction"    (liftQmlResult3 $ executePoiThirdAction sqlBackend),
            defSignalNamedParams "gotOutput"      (Proxy :: Proxy SignalOutput) $ fstName "output"
        ]
    newObject serverViewClass ()
