{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, ViewPatterns #-}
module ServerView where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import Graphics.QML.Objects.ParamNames
import Database.Esqueleto
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Persist as P

import Model
import ModelBase
import ChildEntityHolder
import System
import Util

data ServerViewState = ServerViewState
    {
        curServerId :: MVar (Maybe Int)
    } deriving Typeable

instance DynParentHolder ServerViewState where
    dynParentId = curServerId
readServerPois :: Int -> SqlPersistM [Entity ServerPointOfInterest]
readServerPois serverId = select $ from $ \p -> do
    where_ (p ^. ServerPointOfInterestServerId ==. val (toSqlKey32 serverId))
    orderBy [asc (p ^. ServerPointOfInterestDesc)]
    return p

addServerPoi :: SqlBackend -> ObjRef ServerViewState
    -> Text -> Text -> Text -> Text -> IO ()
addServerPoi sqlBackend stateRef
    pDesc path txt interestTypeT = do
    let interestType = read $ T.unpack interestTypeT
    addHelper sqlBackend stateRef
        $ ServerPointOfInterest pDesc path txt interestType

updateServerPoi :: SqlBackend -> ObjRef (Entity ServerPointOfInterest)
    -> Text -> Text -> Text -> Text -> IO (ObjRef (Entity ServerPointOfInterest))
updateServerPoi sqlBackend poiRef
    pDesc path txt interestTypeT = do
    let interestType = read $ T.unpack interestTypeT
    updateHelper sqlBackend poiRef
        [
            ServerPointOfInterestDesc P.=. pDesc, ServerPointOfInterestPath P.=. path,
            ServerPointOfInterestText P.=. txt,
            ServerPointOfInterestInterestType P.=. interestType
        ]

readServerWebsites :: Int -> SqlPersistM [Entity ServerWebsite]
readServerWebsites serverId = select $ from $ \p -> do
    where_ (p ^. ServerWebsiteServerId ==. val (toSqlKey32 serverId))
    orderBy [asc (p ^. ServerWebsiteDesc)]
    return p

addServerWebsite :: SqlBackend -> ObjRef ServerViewState
    -> Text -> Text -> Text -> Text -> Text -> Maybe Int -> IO ()
addServerWebsite sqlBackend stateRef
    pDesc url txt username password mDatabaseId = do
    let mDatabaseKey = fmap toSqlKey32 mDatabaseId
    addHelper sqlBackend stateRef
        $ ServerWebsite pDesc url txt username password mDatabaseKey

updateServerWebsite :: SqlBackend -> ObjRef (Entity ServerWebsite)
    -> Text -> Text -> Text -> Text -> Text
    -> Maybe Int -> IO (ObjRef (Entity ServerWebsite))
updateServerWebsite sqlBackend srvWwwRef
    pDesc url txt username password mDatabaseId = do
    let mDatabaseKey = fmap toSqlKey32 mDatabaseId
    updateHelper sqlBackend srvWwwRef
        [
            ServerWebsiteDesc P.=. pDesc, ServerWebsiteUrl P.=. url,
            ServerWebsiteText P.=. txt, ServerWebsiteUsername P.=. username,
            ServerWebsitePassword P.=. password,
            ServerWebsiteServerDatabaseId P.=. mDatabaseKey
        ]

readServerDatabases :: Int -> SqlPersistM [Entity ServerDatabase]
readServerDatabases serverId = select $ from $ \p -> do
    where_ (p ^. ServerDatabaseServerId ==. val (toSqlKey32 serverId))
    orderBy [asc (p ^. ServerDatabaseDesc)]
    return p

addServerDatabase :: SqlBackend -> ObjRef ServerViewState
    -> Text -> Text -> Text -> Text -> Text -> IO ()
addServerDatabase sqlBackend stateRef pDesc name txt
    username password = addHelper sqlBackend stateRef
        $ ServerDatabase pDesc name txt username password

updateServerDatabase :: SqlBackend -> ObjRef (Entity ServerDatabase)
    -> Text -> Text -> Text -> Text -> Text -> IO (ObjRef (Entity ServerDatabase))
updateServerDatabase sqlBackend srvDbRef
    pDesc name txt username password = updateHelper sqlBackend srvDbRef
        [
            ServerDatabaseDesc P.=. pDesc, ServerDatabaseName P.=. name,
            ServerDatabaseText P.=. txt,
            ServerDatabaseUsername P.=. username,
            ServerDatabasePassword P.=. password
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

getAllDatabases :: SqlBackend -> ObjRef ServerViewState -> IO [ObjRef (Entity ServerDatabase)]
getAllDatabases sqlBackend _ = do
    dbs <- runSqlBackend sqlBackend (select $ from $ \p -> do
        orderBy [asc (p ^. ServerDatabaseDesc)]
        return p)
    mapM newObjectDC dbs

readServerExtraUserAccounts :: Int -> SqlPersistM [Entity ServerExtraUserAccount]
readServerExtraUserAccounts serverId = select $ from $ \p -> do
    where_ (p ^. ServerExtraUserAccountServerId ==. val (toSqlKey32 serverId))
    orderBy [asc (p ^. ServerExtraUserAccountDesc)]
    return p

addServerExtraUserAccount :: SqlBackend -> ObjRef ServerViewState
    -> Text -> Text -> Text -> Text -> IO ()
addServerExtraUserAccount sqlBackend stateRef
    pDesc username password keyPath = do
    authKeyInfo <- processAuthKeyInfo keyPath
    addHelper sqlBackend stateRef $ ServerExtraUserAccount username password pDesc
            (fst <$> authKeyInfo) (snd <$> authKeyInfo)

updateServerExtraUserAccount :: SqlBackend -> ObjRef (Entity ServerExtraUserAccount)
    -> Text -> Text -> Text -> Text -> IO (ObjRef (Entity ServerExtraUserAccount))
updateServerExtraUserAccount sqlBackend acctRef
    pDesc username password keyPath = do
    authKeyInfo <- processAuthKeyInfo keyPath
    updateHelper sqlBackend acctRef
        [
            ServerExtraUserAccountDesc P.=. pDesc, ServerExtraUserAccountUsername P.=. username,
            ServerExtraUserAccountPassword P.=. password,
            ServerExtraUserAccountAuthKey P.=. fst <$> authKeyInfo,
            ServerExtraUserAccountAuthKeyFilename P.=. snd <$> authKeyInfo
        ]

saveExtraUserAuthKey :: ObjRef ServerViewState
    -> Text -> ObjRef (Entity ServerExtraUserAccount) -> IO (Either Text Text)
saveExtraUserAuthKey _ path (entityVal . fromObjRef -> userAcct) = saveAuthKeyBytes path $
    serverExtraUserAccountAuthKey userAcct

executePoiAction :: ObjRef ServerViewState -> ObjRef (Entity Server)
    -> ObjRef (Entity ServerPointOfInterest) -> IO (Either Text ())
executePoiAction srvState (entityVal . fromObjRef -> server)
        (entityVal . fromObjRef -> serverPoi) =
    case serverPointOfInterestInterestType serverPoi of
        PoiCommandToRun -> executePoiCommand srvState server serverPoi
        PoiLogFile -> executePoiLogFile server serverPoi "tail -f "
        PoiConfigFile -> executePoiLogFile server serverPoi "vim "
        _ -> return $ Right ()

executePoiSecondaryAction :: ObjRef ServerViewState -> ObjRef (Entity Server)
    -> ObjRef (Entity ServerPointOfInterest) -> IO (Either Text ())
executePoiSecondaryAction _ (entityVal . fromObjRef -> server)
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
          x -> Just x
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

createServerViewState :: SqlBackend -> IO (ObjRef ServerViewState)
createServerViewState sqlBackend = do
    serverViewState <- ServerViewState <$> newMVar Nothing
    serverViewClass <- newClass
        [
            defMethod  "getPois" (getChildren sqlBackend readServerPois),
            defMethod  "addServerPoi" (addServerPoi sqlBackend),
            defMethod' "updateServerPoi" (const $ updateServerPoi sqlBackend),
            defMethod' "deleteServerPois" (deleteHelper sqlBackend deleteServerPoi),
            defMethod  "getServerWebsites" (getChildren sqlBackend readServerWebsites),
            defMethod  "addServerWebsite" (addServerWebsite sqlBackend),
            defMethod' "updateServerWebsite" (const $ updateServerWebsite sqlBackend),
            defMethod' "deleteServerWebsites" (deleteHelper sqlBackend deleteServerWebsite),
            defMethod  "getServerDatabases" (getChildren sqlBackend readServerDatabases),
            defMethod  "addServerDatabase" (addServerDatabase sqlBackend),
            defMethod' "updateServerDatabase" (const $ updateServerDatabase sqlBackend),
            defMethod' "canDeleteServerDatabase" (\_ db ->
                canDeleteServerDatabase sqlBackend (const True) $ fromObjRef db),
            defMethod' "deleteServerDatabases" (deleteHelper sqlBackend deleteServerDatabase),
            defMethod  "getAllDatabases" (getAllDatabases sqlBackend),
            defMethod  "getServerExtraUserAccounts" (getChildren sqlBackend readServerExtraUserAccounts),
            defMethod  "addServerExtraUserAccount" (addServerExtraUserAccount sqlBackend),
            defMethod' "updateServerExtraUserAccount" (const $ updateServerExtraUserAccount sqlBackend),
            defMethod' "deleteServerExtraUserAccounts" (deleteHelper sqlBackend deleteServerExtraUserAccount),
            defMethod  "saveAuthKey" (\state path server -> serializeEither <$>
                saveExtraUserAuthKey state path server),
            defMethod' "executePoiAction" (\srvState server serverPoi -> serializeEither' <$>
                executePoiAction srvState server serverPoi),
            defMethod' "executePoiSecondaryAction" (\srvState server serverPoi -> serializeEither' <$>
                executePoiSecondaryAction srvState server serverPoi),
            defSignalNamedParams "gotOutput" (Proxy :: Proxy SignalOutput) $
                fstName "output"
        ]
    newObject serverViewClass serverViewState
