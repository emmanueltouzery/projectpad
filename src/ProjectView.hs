{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ViewPatterns #-}
module ProjectView where

import Control.Applicative
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
import CrudHelpers
import System
import Util
import ServerView

type ProjectViewState = ()

readServers :: Int -> SqlPersistM [Entity Server]
readServers projectId = select $ from $ \s -> do
    where_ (s ^. ServerProjectId ==. val (toSqlKey32 projectId))
    orderBy [asc (s ^. ServerDesc)]
    return s

addServer :: SqlBackend -> Int
    -> Text -> IpAddress -> Text -> Text -> Text -> Text
    -> Text -> Text -> Text -> Maybe Text -> IO ()
addServer sqlBackend projectId sDesc ipAddr txt username password
        keyPath serverTypeT serverAccessTypeT srvEnvironmentT
        (groupOrNothing -> groupName) = do
    let srvType       = readT serverTypeT
    let srvAccessType = readT serverAccessTypeT
    let srvEnv        = readT srvEnvironmentT
    authKeyInfo <- processAuthKeyInfo keyPath
    addHelper sqlBackend projectId $ Server sDesc ipAddr txt username password
            (fst <$> authKeyInfo) (snd <$> authKeyInfo)
            srvType srvAccessType srvEnv groupName

updateServer :: SqlBackend -> ObjRef (Entity Server)
    -> Text -> IpAddress -> Text -> Text -> Text -> Text -> Text
    -> Text -> Maybe Text -> IO (ObjRef (Entity Server))
updateServer sqlBackend serverRef sDesc ipAddr txt
  username password keyPath serverTypeT serverAccessTypeT
  (groupOrNothing -> groupName) = do
    let srvType       = read $ T.unpack serverTypeT
    let srvAccessType = read $ T.unpack serverAccessTypeT
    authKeyInfo <- processAuthKeyInfo keyPath
    updateHelper sqlBackend serverRef
        [
            ServerDesc P.=. sDesc, ServerIp P.=. ipAddr,
            ServerText P.=. txt,
            ServerUsername P.=. username, ServerPassword P.=. password,
            ServerType P.=. srvType, ServerAccessType P.=. srvAccessType,
            ServerAuthKey P.=. fst <$> authKeyInfo,
            ServerAuthKeyFilename P.=. snd <$> authKeyInfo,
            ServerGroupName P.=. groupName
        ]

readPois :: Int -> SqlPersistM [Entity ProjectPointOfInterest]
readPois projectId = select $ from $ \poi -> do
    where_ (poi ^. ProjectPointOfInterestProjectId ==. val (toSqlKey32 projectId))
    orderBy [asc (poi ^. ProjectPointOfInterestDesc)]
    return poi

addProjectPoi :: SqlBackend -> Int
    -> Text -> Text -> Text -> Text -> Maybe Text -> IO ()
addProjectPoi sqlBackend projectId
    pDesc path txt interestTypeT (groupOrNothing -> groupName) = do
    let interestType = read $ T.unpack interestTypeT
    addHelper sqlBackend projectId
        $ ProjectPointOfInterest pDesc path txt interestType groupName

updateProjectPoi :: SqlBackend -> ObjRef (Entity ProjectPointOfInterest)
    -> Text -> Text -> Text -> Text -> Maybe Text
    -> IO (ObjRef (Entity ProjectPointOfInterest))
updateProjectPoi sqlBackend poiRef
    pDesc path txt interestTypeT (groupOrNothing -> groupName) = do
    let interestType = read $ T.unpack interestTypeT
    updateHelper sqlBackend poiRef
        [
            ProjectPointOfInterestDesc P.=. pDesc, ProjectPointOfInterestPath P.=. path,
            ProjectPointOfInterestText P.=. txt,
            ProjectPointOfInterestInterestType P.=. interestType,
            ProjectPointOfInterestGroupName P.=. groupName
        ]

getProjectGroupNames :: SqlBackend -> Int -> IO [Text]
getProjectGroupNames sqlBackend projectId = do
    serverGroupNames      <- readEF readServers serverGroupName
    projectPoisGroupNames <- readEF readPois projectPointOfInterestGroupName
    return $ mergeNames $ serverGroupNames ++ projectPoisGroupNames
    where
      readEF :: (Int -> SqlPersistM [Entity a]) -> (a -> Maybe Text) -> IO [Maybe Text]
      readEF f = readEntityField sqlBackend (f projectId)

data ProjectDisplaySection = ProjectDisplaySection
    {
        prjSectionGrpName     :: Maybe Text,
        prjSectionServers     :: [ObjRef ServerExtraInfo],
        prjSectionProjectPois :: [ObjRef (Entity ProjectPointOfInterest)]
    } deriving Typeable

instance DefaultClass ProjectDisplaySection where
    classMembers =
        [
            defPropertyConst "groupName" (readM prjSectionGrpName),
            defPropertyConst "servers"   (readM prjSectionServers),
            defPropertyConst "pois"      (readM prjSectionProjectPois)
        ]

getProjectDisplaySections :: SqlBackend -> Int -> Text -> IO [ObjRef ProjectDisplaySection]
getProjectDisplaySections sqlBackend projectId environment = do
    groupNames <- getProjectGroupNames sqlBackend projectId
    servers    <- getServersExtraInfo sqlBackend environment projectId
    pois       <- runServerQ readPois
    let sectionForGroup grp = ProjectDisplaySection {
            prjSectionGrpName     = grp,
            prjSectionServers     =
                filter ((== grp) . serverGroupName . srvExtraInfoRefGetServer) servers,
            prjSectionProjectPois =
                filterForGroup grp projectPointOfInterestGroupName pois
        }
    mapM newObjectDC $ sectionForGroup Nothing : (sectionForGroup . Just <$> groupNames)
    where
      runServerQ :: DefaultClass a => (Int -> SqlPersistM [a]) -> IO [ObjRef a]
      runServerQ f = sqlToQml sqlBackend (f projectId)
      srvExtraInfoRefGetServer = entityVal . fromObjRef . srvExtraInfoServer . fromObjRef

splitParams :: Text -> Either String [Text]
splitParams = eitherResult . flip feed T.empty . parse splitParamsParser
    where
        splitParamsParser = (parseQuotedParam <|> parseParam) `sepBy` char ' '
        parseQuotedParam = char '"' *> takeWhile1 (/= '"') <* char '"'
        parseParam = takeWhile1 (/= ' ')

runPoiAction :: ObjRef ProjectViewState
    -> ObjRef (Entity ProjectPointOfInterest) -> IO ()
runPoiAction prjViewState (entityVal . fromObjRef -> poi)
    | interest == PoiCommandToRun = case splitParams txt of
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
        srvExtraInfoServer    :: ObjRef (Entity Server),
        srvExtraInfoPoiCount  :: Int,
        srvExtraInfoWwwCount  :: Int,
        srvExtraInfoDbCount   :: Int,
        srvExtraInfoUserCount :: Int
    } deriving Typeable

-- server entity, together with some stats about the
-- entities inside the server.
instance DefaultClass ServerExtraInfo where
    classMembers =
        [
            defPropertyConst "server"    (readM srvExtraInfoServer),
            defPropertyConst "poiCount"  (readM srvExtraInfoPoiCount),
            defPropertyConst "wwwCount"  (readM srvExtraInfoWwwCount),
            defPropertyConst "dbCount"   (readM srvExtraInfoDbCount),
            defPropertyConst "userCount" (readM srvExtraInfoUserCount)
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

getServersExtraInfo :: SqlBackend -> Text -> Int -> IO [ObjRef ServerExtraInfo]
getServersExtraInfo sqlBackend environment projectId = do
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
    projectViewClass <- newClass
        [
            defStatic  "getProjectDisplaySections" (getProjectDisplaySections sqlBackend),
            defStatic  "getServers"        (getServersExtraInfo sqlBackend),
            defStatic  "getProjectGroupNames" (getProjectGroupNames sqlBackend),
            defStatic  "addServer"         (addServer sqlBackend),
            defStatic  "updateServer"      (updateServer sqlBackend),
            defStatic  "canDeleteServer"   (canDeleteServer sqlBackend . fromObjRef),
            defMethod' "deleteServers"     (deleteHelper sqlBackend deleteServer),
            defStatic  "addProjectPoi"     (addProjectPoi sqlBackend),
            defStatic  "updateProjectPoi"  (updateProjectPoi sqlBackend),
            defMethod' "deleteProjectPois" (deleteHelper sqlBackend deleteProjectPoi),
            defMethod' "runPoiAction"      runPoiAction,
            defStatic  "saveAuthKey"       (liftQmlResult2 saveAuthKey),
            defStatic  "runRdp"            (liftQmlResult3 runServerRdp),
            defStatic  "openSshSession"    (liftQmlResult1 openServerSshSession),
            defSignalNamedParams "gotOutput" (Proxy :: Proxy SignalOutput) $
                                                 fstName "output"
        ]
    newObject projectViewClass ()
