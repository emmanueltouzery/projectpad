{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ViewPatterns #-}
module ProjectView where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import Graphics.QML.Objects.ParamNames
import Database.Esqueleto
import qualified Database.Persist as P
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe
import Control.Arrow
import Control.Error
import Data.Monoid
import Data.Attoparsec.Text hiding (count)

import ModelBase
import Model
import ChildEntityHolder
import System
import Util
import ServerView

data ProjectViewState = ProjectViewState
    {
        curProjectId :: MVar (Maybe Int)
    } deriving Typeable

instance DynParentHolder ProjectViewState where
    dynParentId = curProjectId

readServers :: Int -> SqlPersistM [Entity Server]
readServers projectId = select $ from $ \s -> do
    where_ (s ^. ServerProjectId ==. val (toSqlKey32 projectId))
    orderBy [asc (s ^. ServerDesc)]
    return s

addServer :: SqlBackend -> Int
    -> Text -> IpAddress -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> IO ()
addServer sqlBackend projectId sDesc ipAddr txt username password
        keyPath serverTypeT serverAccessTypeT srvEnvironmentT = do
    let srvType = readT serverTypeT
    let srvAccessType = readT serverAccessTypeT
    let srvEnv = readT srvEnvironmentT
    authKeyInfo <- processAuthKeyInfo keyPath
    addHelper sqlBackend projectId $ Server sDesc ipAddr txt username password
            (fst <$> authKeyInfo) (snd <$> authKeyInfo)
            srvType srvAccessType srvEnv

updateServer :: SqlBackend -> ObjRef (Entity Server)
    -> Text -> IpAddress -> Text -> Text -> Text -> Text -> Text
    -> Text -> IO (ObjRef (Entity Server))
updateServer sqlBackend serverRef sDesc ipAddr txt
  username password keyPath serverTypeT serverAccessTypeT = do
    let srvType = read $ T.unpack serverTypeT
    let srvAccessType = read $ T.unpack serverAccessTypeT
    authKeyInfo <- processAuthKeyInfo keyPath
    updateHelper sqlBackend serverRef
        [
            ServerDesc P.=. sDesc, ServerIp P.=. ipAddr,
            ServerText P.=. txt,
            ServerUsername P.=. username, ServerPassword P.=. password,
            ServerType P.=. srvType, ServerAccessType P.=. srvAccessType,
            ServerAuthKey P.=. fst <$> authKeyInfo,
            ServerAuthKeyFilename P.=. snd <$> authKeyInfo
        ]

readPois :: Int -> SqlPersistM [Entity ProjectPointOfInterest]
readPois projectId = select $ from $ \poi -> do
    where_ (poi ^. ProjectPointOfInterestProjectId ==. val (toSqlKey32 projectId))
    orderBy [asc (poi ^. ProjectPointOfInterestDesc)]
    return poi

addProjectPoi :: SqlBackend -> Int
    -> Text -> Text -> Text -> Text -> IO ()
addProjectPoi sqlBackend projectId
    pDesc path txt interestTypeT = do
    let interestType = read $ T.unpack interestTypeT
    addHelper sqlBackend projectId
        $ ProjectPointOfInterest pDesc path txt interestType

updateProjectPoi :: SqlBackend -> ObjRef (Entity ProjectPointOfInterest)
    -> Text -> Text -> Text -> Text -> IO (ObjRef (Entity ProjectPointOfInterest))
updateProjectPoi sqlBackend poiRef
    pDesc path txt interestTypeT = do
    let interestType = read $ T.unpack interestTypeT
    updateHelper sqlBackend poiRef
        [
            ProjectPointOfInterestDesc P.=. pDesc, ProjectPointOfInterestPath P.=. path,
            ProjectPointOfInterestText P.=. txt,
            ProjectPointOfInterestInterestType P.=. interestType
        ]

splitParams :: Text -> Either String [Text]
splitParams = eitherResult . flip feed T.empty . parse splitParamsParser
    where
        splitParamsParser = (parseQuotedParam <|> parseParam) `sepBy` char ' '
        parseQuotedParam = char '"' *> takeWhile1 (/= '"') <* char '"'
        parseParam = takeWhile1 (/= ' ')

runPoiAction :: ObjRef ProjectViewState
    -> ObjRef (Entity ProjectPointOfInterest) -> IO ()
runPoiAction prjViewState (entityVal . fromObjRef -> poi)
    | interest == PoiCommandToRun = case fmap T.unpack <$> splitParams txt of
        Left x -> notify (CommandFailed $ "Error parsing the command: " <> T.pack x)
        Right [] -> notify (CommandFailed "Incomplete command line")
        Right (prog:parameters) -> tryCommandAsync prog parameters path Nothing notify
    | interest `elem` [PoiLogFile, PoiApplication] = do
        result <- openAssociatedFile (projectPointOfInterestPath poi)
        notify (eitherToCmdProgress result)
    | otherwise = putStrLn "poi action not handled"
    where
        notify = fireSignal (Proxy :: Proxy SignalOutput)
            prjViewState . cmdProgressToJs
        interest = projectPointOfInterestInterestType poi
        path = case T.unpack $ projectPointOfInterestPath poi of
            "" -> Nothing
            x@_ -> Just x
        txt = projectPointOfInterestText poi

saveAuthKey :: Text -> ObjRef (Entity Server) -> IO (Either Text Text)
saveAuthKey path (entityVal . fromObjRef -> server) =
    saveAuthKeyBytes path (serverAuthKey server)

runServerRdp :: ObjRef (Entity Server) -> Int -> Int -> IO (Either Text Text)
runServerRdp (entityVal . fromObjRef -> server) =
    runRdp (serverIp server) (serverUsername server) (serverPassword server)

openServerSshSession :: ObjRef (Entity Server) -> IO (Either Text Text)
openServerSshSession (entityVal . fromObjRef -> server) = fmapR (const "") <$>
    openSshSession (serverIp server) (serverUsername server) (serverPassword server) Nothing

data ServerExtraInfo = ServerExtraInfo
    {
        srvExtraInfoServer :: ObjRef (Entity Server),
        srvExtraInfoPoiCount :: Int,
        srvExtraInfoWwwCount :: Int,
        srvExtraInfoDbCount :: Int,
        srvExtraInfoUserCount :: Int
    } deriving Typeable

-- server entity, together with some stats about the
-- entities inside the server.
instance DefaultClass ServerExtraInfo where
    classMembers =
        [
            defPropertyConst "server" (return . srvExtraInfoServer . fromObjRef),
            defPropertyConst "poiCount" (return . srvExtraInfoPoiCount . fromObjRef),
            defPropertyConst "wwwCount" (return . srvExtraInfoWwwCount . fromObjRef),
            defPropertyConst "dbCount" (return . srvExtraInfoDbCount . fromObjRef),
            defPropertyConst "userCount" (return . srvExtraInfoUserCount . fromObjRef)
        ]

readServersExtraInfo :: (PersistEntity val,
    PersistField typ1, PersistField typ, PersistEntityBackend val ~ SqlBackend) =>
    EntityField val typ1 -> EntityField val typ -> [typ] -> SqlPersistM [(Value typ, Value Int)]
readServersExtraInfo tableId serverFk serverIds = select $ from $ \sp -> do
        groupBy (sp ^. serverFk)
        having (sp ^. serverFk `in_` valList serverIds)
        return (sp ^. serverFk, count (sp ^. tableId))

getInfosVal :: (PersistEntity val, PersistField typ1, PersistEntityBackend val ~ SqlBackend)
    => SqlBackend -> M.Map (Key Server) a -> EntityField val typ1
    -> EntityField val (Key Server) -> IO (M.Map (Key Server) Int)
getInfosVal sqlBackend serversById tableId serverFk = do
    poiInfosVal <- runSqlBackend sqlBackend $ readServersExtraInfo
        tableId serverFk $ M.keys serversById
    return $ M.fromList $ fmap (unValue *** unValue) poiInfosVal

getServersExtraInfo :: SqlBackend -> Int -> Text -> IO [ObjRef ServerExtraInfo]
getServersExtraInfo sqlBackend projectId environment = do
    let environmentType = readT environment
    prjServers <- runSqlBackend sqlBackend $ readServers projectId
    let envServers = filter ((==environmentType) . serverEnvironment . entityVal) prjServers
    let serversById = M.fromList $ fmap (\s -> (entityKey s, s)) envServers

    poiInfos <- getInfosVal sqlBackend serversById
        ServerPointOfInterestId ServerPointOfInterestServerId
    wwwInfos <- getInfosVal sqlBackend serversById
        ServerWebsiteId ServerWebsiteServerId
    dbInfos <- getInfosVal sqlBackend serversById
        ServerDatabaseId ServerDatabaseServerId
    userInfos <- getInfosVal sqlBackend serversById
        ServerExtraUserAccountId ServerExtraUserAccountServerId

    mapM (\s -> do
               s' <- newObjectDC s
               newObjectDC $ ServerExtraInfo s'
                   (getServerCount s poiInfos)
                   (getServerCount s wwwInfos)
                   (getServerCount s dbInfos)
                   (getServerCount s userInfos)) envServers
    where
        getServerCount s = fromMaybe 0 . M.lookup (entityKey s)

canDeleteServer :: SqlBackend -> Entity Server -> IO (Maybe Text)
canDeleteServer sqlBackend server = do
    dbs <- runSqlBackend sqlBackend
           (readServerDatabases $ fromSqlKey32 server)
    -- allow to delete databases even if websites refer to them, so long
    -- as they refer to them from that same server (so they'll be deleted too).
    messages <- catMaybes <$> mapM (canDeleteServerDatabase sqlBackend (/= entityKey server)) dbs
    return $ case messages of
     [] -> Nothing
     l  -> Just (T.intercalate ", " l)

deleteServer :: Key Server -> SqlPersistM ()
deleteServer = P.delete

deleteProjectPoi :: Key ProjectPointOfInterest -> SqlPersistM ()
deleteProjectPoi = P.delete

createProjectViewState :: SqlBackend -> IO (ObjRef ProjectViewState)
createProjectViewState sqlBackend = do
    projectViewState <- ProjectViewState <$> newMVar Nothing
    projectViewClass <- newClass
        [
            defStatic  "getServers" (getServersExtraInfo sqlBackend),
            defStatic  "addServer" (addServer sqlBackend),
            defMethod' "updateServer" (const $ updateServer sqlBackend),
            defStatic  "canDeleteServer" (canDeleteServer sqlBackend . fromObjRef),
            defMethod' "deleteServers" (deleteHelper sqlBackend deleteServer),
            defMethod  "getPois" (getChildren sqlBackend readPois),
            defStatic  "addProjectPoi" (addProjectPoi sqlBackend),
            defMethod' "updateProjectPoi" (const $ updateProjectPoi sqlBackend),
            defMethod' "deleteProjectPois" (deleteHelper sqlBackend deleteProjectPoi),
            defMethod' "runPoiAction" runPoiAction,
            defStatic  "saveAuthKey" (serializeEitherM . saveAuthKey),
            defStatic  "runRdp" (\server width height -> serializeEither <$>
                runServerRdp server width height),
            defStatic  "openSshSession" (\server -> serializeEither <$>
                openServerSshSession server),
            defSignalNamedParams "gotOutput" (Proxy :: Proxy SignalOutput) $
                fstName "output"
        ]
    newObject projectViewClass projectViewState
