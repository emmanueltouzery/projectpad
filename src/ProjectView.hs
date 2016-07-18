{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, NoMonoLocalBinds,
    MultiParamTypeClasses, FlexibleContexts, ViewPatterns, ConstraintKinds #-}
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

addServer :: SqlBackend -> Int -> Text -> IpAddress -> Text -> Text
    -> Text -> Text -> Text -> Text -> Maybe Int -> Maybe Int
    -> Text -> Maybe Text -> IO ()
addServer sqlBackend projectId sDesc ipAddr txt username password
        keyPath serverTypeT serverAccessTypeT sshTunnelPort sshTunnelThroughServerId
        srvEnvironmentT (groupOrNothing -> groupName) = do
    let srvType       = readT serverTypeT
    let srvAccessType = readT serverAccessTypeT
    let srvEnv        = readT srvEnvironmentT
    let mTunnelThroughId = (toSqlKey . fromIntegral) <$> sshTunnelThroughServerId
    authKeyInfo <- processAuthKeyInfo keyPath
    addHelper sqlBackend projectId $ Server sDesc ipAddr txt username password
            (fst <$> authKeyInfo) (snd <$> authKeyInfo)
            srvType srvAccessType sshTunnelPort mTunnelThroughId srvEnv groupName

updateServer :: SqlBackend -> EntityRef Server
    -> Text -> IpAddress -> Text -> Text -> Text -> Text -> Text
    -> Text -> Maybe Int -> Maybe Int -> Maybe Text
    -> IO (EntityRef Server)
updateServer sqlBackend serverRef sDesc ipAddr txt
  username password keyPath serverTypeT serverAccessTypeT
  sshTunnelPort sshTunnelThroughServerId (groupOrNothing -> groupName) = do
    let srvType       = read $ T.unpack serverTypeT
    let srvAccessType = read $ T.unpack serverAccessTypeT
    let mTunnelThroughId = (toSqlKey . fromIntegral) <$> sshTunnelThroughServerId
    authKeyInfo <- processAuthKeyInfo keyPath
    updateHelper sqlBackend serverRef
        [
            ServerDesc P.=. sDesc, ServerIp P.=. ipAddr,
            ServerText P.=. txt,
            ServerUsername P.=. username, ServerPassword P.=. password,
            ServerType P.=. srvType, ServerAccessType P.=. srvAccessType,
            ServerAuthKey P.=. fst <$> authKeyInfo,
            ServerAuthKeyFilename P.=. snd <$> authKeyInfo,
            ServerSshTunnelPort P.=. sshTunnelPort,
            ServerSshTunnelThroughServerId P.=. mTunnelThroughId,
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

updateProjectPoi :: SqlBackend -> EntityRef ProjectPointOfInterest
    -> Text -> Text -> Text -> Text -> Maybe Text
    -> IO (EntityRef ProjectPointOfInterest)
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

readNotes :: Int -> SqlPersistM [Entity ProjectNote]
readNotes projectId = select $ from $ \nte -> do
    where_ (nte ^. ProjectNoteProjectId ==. val (toSqlKey32 projectId))
    orderBy [asc (nte ^. ProjectNoteTitle)]
    return nte

addProjectNote :: SqlBackend -> Int -> Text -> Text -> Maybe Text -> IO ()
addProjectNote sqlBackend projectId title contents
    (groupOrNothing -> groupName) = addHelper sqlBackend projectId
        $ ProjectNote title contents groupName

updateProjectNote :: SqlBackend -> EntityRef ProjectNote
    -> Text -> Text -> Maybe Text
    -> IO (EntityRef ProjectNote)
updateProjectNote sqlBackend noteRef title contents
    (groupOrNothing -> groupName) = updateHelper sqlBackend noteRef
        [
            ProjectNoteTitle P.=. title, ProjectNoteContents P.=. contents,
            ProjectNoteGroupName P.=. groupName
        ]

readServerForLink :: SqlBackend -> ServerLink -> IO (Entity Server)
readServerForLink sqlBackend serverLink = fromMaybe (error "no server for serverlink?") .
    listToMaybe <$> runSqlBackend sqlBackend q
    where
      q = select $ from $ \s -> do
              where_ (s ^. ServerId ==. val (serverLinkLinkedServerId serverLink))
              limit 1
              return s

readServerLinks :: Int -> SqlPersistM [Entity ServerLink]
readServerLinks projectId = select $ from $ \nte -> do
    where_ (nte ^. ServerLinkProjectId ==. val (toSqlKey32 projectId))
    orderBy [asc (nte ^. ServerLinkDesc)]
    return nte

addServerLink :: SqlBackend -> Int -> Text -> Int -> Text -> Maybe Text -> IO ()
addServerLink sqlBackend projectId lnkDesc linkedServerIntId
    srvEnvironmentT (groupOrNothing -> groupName) = do
    let srvEnv        = readT srvEnvironmentT
    let linkedServerId = toSqlKey $ fromIntegral linkedServerIntId
    addHelper sqlBackend projectId $ ServerLink lnkDesc linkedServerId srvEnv groupName

updateServerLink :: SqlBackend -> EntityRef ServerLink
    -> Text -> Int -> Maybe Text -> IO (EntityRef ServerLink)
updateServerLink sqlBackend serverLinkRef lnkDesc
    linkedServerIntId (groupOrNothing -> groupName) =
    updateHelper sqlBackend serverLinkRef
        [
            ServerLinkDesc P.=. lnkDesc,
            ServerLinkLinkedServerId P.=. toSqlKey (fromIntegral linkedServerIntId),
            ServerLinkGroupName P.=. groupName
        ]

getProjectGroupNames :: SqlBackend -> Int -> IO [Text]
getProjectGroupNames sqlBackend projectId = do
    serverGroupNames      <- readEF readServers serverGroupName
    projectPoisGroupNames <- readEF readPois projectPointOfInterestGroupName
    return $ mergeNames $ serverGroupNames ++ projectPoisGroupNames
    where
      readEF f = readEntityFields sqlBackend (f projectId)

data ProjectDisplaySection = ProjectDisplaySection
    {
        prjSectionGrpName       :: Maybe Text,
        prjSectionServers       :: [ObjRef ServerExtraInfo],
        prjSectionLinkedServers :: [ObjRef ServerLinkInfo],
        prjSectionProjectPois   :: [EntityRef ProjectPointOfInterest],
        prjSectionNotes         :: [EntityRef ProjectNote]
    } deriving Typeable

instance DefaultClass ProjectDisplaySection where
    classMembers =
        [
            defPropertyConst "groupName"     (readM prjSectionGrpName),
            defPropertyConst "servers"       (readM prjSectionServers),
            defPropertyConst "linkedServers" (readM prjSectionLinkedServers),
            defPropertyConst "pois"          (readM prjSectionProjectPois),
            defPropertyConst "notes"         (readM prjSectionNotes)
        ]

getProjectDisplaySections :: SqlBackend -> Int -> Text -> IO [ObjRef ProjectDisplaySection]
getProjectDisplaySections sqlBackend projectId environment = do
    groupNames    <- getProjectGroupNames sqlBackend projectId
    servers       <- getProjectServersExtraInfo sqlBackend environment projectId
    linkedServers <- getProjectServerLinks sqlBackend environment projectId
    pois          <- runServerQ readPois
    notes         <- runServerQ readNotes
    let sectionForGroup grp = ProjectDisplaySection {
            prjSectionGrpName = grp,
            prjSectionServers =
                  filter ((== grp) . serverGroupName . fromEntRef srvExtraInfoServer) servers,
            prjSectionLinkedServers =
                  filter ((==grp) . serverLinkGroupName . fromEntRef srvLinkInfoServerLink) linkedServers,
            prjSectionProjectPois =
                  filterForGroup grp projectPointOfInterestGroupName pois,
            prjSectionNotes = filterForGroup grp projectNoteGroupName notes
        }
    mapM newObjectDC $ sectionForGroup Nothing : (sectionForGroup . Just <$> groupNames)
    where
      runServerQ :: DefaultClass a => (Int -> SqlPersistM [a]) -> IO [ObjRef a]
      runServerQ f = sqlToQml sqlBackend (f projectId)
      fromEntRef f = fromEntityRef . f . fromObjRef

splitParams :: Text -> Either String [Text]
splitParams = parseOnly splitParamsParser
    where
        splitParamsParser = (parseQuotedParam <|> parseParam) `sepBy` char ' ' <* endOfInput
        parseQuotedParam = char '"' *> takeWhile1 (/= '"') <* char '"'
        parseParam = takeWhile1 (/= ' ')

withParams :: Text -> (CommandProgress -> a) -> ([Text] -> a) -> a
withParams txt notify action = case splitParams txt of
  Left x   -> notify (CommandFailed $ "Error parsing the command: " <> T.pack x)
  Right [] -> notify (CommandFailed "Incomplete command line")
  Right params -> action params

runPoiAction :: ObjRef ProjectViewState
    -> EntityRef ProjectPointOfInterest -> IO ()
runPoiAction prjViewState (fromEntityRef -> poi)
    | interest == PoiCommandToRun = withParams txt notify $
      \(prog:parameters) -> tryCommandAsync prog parameters path Nothing notify
    | interest == PoiCommandTerminal = tryCommandAsync "gnome-terminal" ["-e", txt] path Nothing notify
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

saveServerAuthKey :: EntityRef Server -> IO (Either Text Text)
saveServerAuthKey = saveAuthKey serverAuthKeyFilename serverAuthKey

runServerRdp :: EntityRef Server -> Int -> Int -> IO (Either Text Text)
runServerRdp (fromEntityRef -> server) = runRdp (serverToSystemServer server)

openServerSshSession :: SqlBackend -> EntityRef Server -> IO (Either Text ())
openServerSshSession sqlBackend server = openServerSshAction sqlBackend server $
    \port srv -> openSshSession srv port JustSsh

data ServerExtraInfo = ServerExtraInfo
    {
        srvExtraInfoServer    :: EntityRef Server,
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

data ServerLinkInfo = ServerLinkInfo
    {
        srvLinkInfoServerLink :: EntityRef ServerLink,
        srvLinkInfoServer     :: EntityRef Server
    }

instance DefaultClass ServerLinkInfo where
    classMembers =
        [
            defPropertyConst "server"     (readM srvLinkInfoServer),
            defPropertyConst "serverLink" (readM srvLinkInfoServerLink)
        ]

readServersExtraInfo :: (SqlEntity val, PersistField typ1, PersistField typ) =>
    EntityField val typ1 -> EntityField val typ -> [typ] -> SqlPersistM [(Value typ, Value Int)]
readServersExtraInfo tableId serverFk serverIds = select $ from $ \sp -> do
        groupBy (sp ^. serverFk)
        having  (sp ^. serverFk `in_` valList serverIds)
        return  (sp ^. serverFk, count (sp ^. tableId))

getInfosVal :: (SqlEntity val, PersistField typ1) =>
    SqlBackend -> [Key Server] -> EntityField val typ1
    -> EntityField val (Key Server) -> IO (M.Map (Key Server) Int)
getInfosVal sqlBackend serverIds tableId serverFk = do
    poiInfosVal <- runSqlBackend sqlBackend $ readServersExtraInfo
        tableId serverFk serverIds
    return $ M.fromList $ fmap (unValue *** unValue) poiInfosVal

getProjectServersExtraInfo :: SqlBackend -> Text -> Int -> IO [ObjRef ServerExtraInfo]
getProjectServersExtraInfo sqlBackend environment projectId = do
    let environmentType = readT environment
    prjServers <- runSqlBackend sqlBackend $ readServers projectId
    let envServers = filter ((==environmentType) . serverEnvironment . entityVal) prjServers
    getServersExtraInfo sqlBackend envServers

getServersExtraInfo :: SqlBackend -> [Entity Server] -> IO [ObjRef ServerExtraInfo]
getServersExtraInfo sqlBackend servers = do
    let serverIds = entityKey <$> servers
    poiInfos <- getInfosVal sqlBackend serverIds
        ServerPointOfInterestId ServerPointOfInterestServerId
    wwwInfos <- getInfosVal sqlBackend serverIds
        ServerWebsiteId ServerWebsiteServerId
    dbInfos <- getInfosVal sqlBackend serverIds
        ServerDatabaseId ServerDatabaseServerId
    userInfos <- getInfosVal sqlBackend serverIds
        ServerExtraUserAccountId ServerExtraUserAccountServerId

    mapM (\s -> do
               s' <- newObjectDC s
               newObjectDC $ ServerExtraInfo s'
                   (getServerCount s poiInfos)
                   (getServerCount s wwwInfos)
                   (getServerCount s dbInfos)
                   (getServerCount s userInfos)) servers
    where
        getServerCount s = fromMaybe 0 . M.lookup (entityKey s)

serverLinksGetInfo :: SqlBackend -> [Entity ServerLink] -> IO [ObjRef ServerLinkInfo]
serverLinksGetInfo sqlBackend serverLinks = do
    linkedServers <- mapM (readServerForLink sqlBackend) (entityVal <$> serverLinks)
    mapM newObjectDC =<< (zipWith ServerLinkInfo
                          <$> mapM newObjectDC serverLinks
                          <*> mapM newObjectDC linkedServers)

getProjectServerLinks :: SqlBackend -> Text -> Int -> IO [ObjRef ServerLinkInfo]
getProjectServerLinks sqlBackend environment projectId = do
    let envType = readT environment
    serverLinks <- runSqlBackend sqlBackend (readServerLinks projectId)
    let envServerLinks = filter ((==envType) . serverLinkEnvironment . entityVal) serverLinks
    serverLinksGetInfo sqlBackend envServerLinks

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

deleteProjectNote :: Key ProjectNote -> SqlPersistM ()
deleteProjectNote = P.delete

deleteServerLink :: Key ServerLink -> SqlPersistM ()
deleteServerLink = P.delete

createProjectViewState :: SqlBackend -> IO (ObjRef ProjectViewState)
createProjectViewState sqlBackend = do
    projectViewClass <- newClass
        [
            defStatic  "getProjectDisplaySections" (getProjectDisplaySections sqlBackend),
            defStatic  "getServers"        (getProjectServersExtraInfo sqlBackend),
            defStatic  "getProjectGroupNames" (getProjectGroupNames sqlBackend),
            defStatic  "addServer"         (addServer sqlBackend),
            defStatic  "updateServer"      (updateServer sqlBackend),
            defStatic  "canDeleteServer"   (canDeleteServer sqlBackend . fromObjRef),
            defMethod' "deleteServers"     (deleteHelper sqlBackend deleteServer),
            defStatic  "addProjectPoi"     (addProjectPoi sqlBackend),
            defStatic  "updateProjectPoi"  (updateProjectPoi sqlBackend),
            defMethod' "deleteProjectPois" (deleteHelper sqlBackend deleteProjectPoi),
            defStatic  "addProjectNote"    (addProjectNote sqlBackend),
            defStatic  "updateProjectNote" (updateProjectNote sqlBackend),
            defMethod' "deleteProjectNotes" (deleteHelper sqlBackend deleteProjectNote),
            defStatic  "addServerLink"     (addServerLink sqlBackend),
            defStatic  "updateServerLink"  (updateServerLink sqlBackend),
            defMethod' "deleteServerLinks" (deleteHelper sqlBackend deleteServerLink),
            defMethod' "runPoiAction"      runPoiAction,
            defStatic  "saveAuthKey"       (liftQmlResult1 saveServerAuthKey),
            defStatic  "runRdp"            (liftQmlResult3 runServerRdp),
            defStatic  "openSshSession"    (liftQmlResult1 $ openServerSshSession sqlBackend),
            defSignalNamedParams "gotOutput" (Proxy :: Proxy SignalOutput) $
                                                 fstName "output"
        ]
    newObject projectViewClass ()
