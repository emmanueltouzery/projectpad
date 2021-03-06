{-# LANGUAGE OverloadedStrings, TypeFamilies, ViewPatterns,
    NoMonomorphismRestriction, NoMonoLocalBinds, FlexibleContexts #-}
module ProjectList where

import Graphics.QML
import qualified Database.Persist as P
import Database.Esqueleto
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe hiding (isNothing)
import Control.Monad
import System.FilePath.Posix
import System.Directory
import Data.Monoid

import Model
import CrudHelpers
import Util
import System

type ProjectListState = ()

readProjects :: SqlPersistM [Entity Project]
readProjects = select $ from $ \p -> do
    orderBy [asc (p ^. ProjectName)]
    return p

readIcon :: Maybe Text -> IO BS.ByteString
readIcon mPath = case mPath >>= T.stripPrefix "file://" of
    Nothing   -> return BS.empty
    Just path -> BS.readFile (T.unpack path)

addProject :: SqlBackend -> SignalKey (IO ()) -> ObjRef ProjectListState
           -> Text -> Maybe Text -> Bool -> Bool -> Bool -> Bool -> IO ()
addProject sqlBackend changeKey state txt iconPath hasDev hasUat hasStage hasProd = do
    -- a project must have at least one environment. Refuse to update otherwise.
    iconBytes <- readIcon iconPath
    when (hasDev || hasUat || hasStage || hasProd) $ void $
        runSqlBackend sqlBackend $ P.insert (Project txt iconBytes
            hasDev hasUat hasStage hasProd)
    fireSignal changeKey state

updateProject :: SqlBackend -> SignalKey (IO ()) -> ObjRef ProjectListState
    -> EntityRef Project -> Text -> Maybe Text -> Bool -> Bool
    -> Bool -> Bool -> IO (EntityRef Project)
updateProject sqlBackend changeKey state prj name iconPath hasDev hasUat hasStage hasProd = do
    let idKey = entityKey $ fromObjRef prj
    iconBytes <- readIcon iconPath
    runSqlBackend sqlBackend $ P.update idKey [ProjectName P.=. name,
        ProjectHasDev P.=. hasDev, ProjectHasUat P.=. hasUat,
        ProjectHasStage P.=. hasStage, ProjectHasProd P.=. hasProd,
        ProjectIcon P.=. iconBytes]
    fireSignal changeKey state
    newProjectList <- runSqlBackend sqlBackend readProjects
    let mUpdatedProjectEntity = find ((== idKey) . entityKey) newProjectList
    newObjectDC $ fromMaybe (error "Can't find project after update?") mUpdatedProjectEntity

deleteProject :: Key Project -> SqlPersistM ()
deleteProject = P.delete

deleteProjects :: SqlBackend -> SignalKey (IO ()) -> ObjRef ProjectListState
    -> [Int] -> IO (ObjRef (QmlResult ()))
deleteProjects sqlBackend changeKey state projectIds = do
    r <- deleteHelper sqlBackend deleteProject state projectIds
    fireSignal changeKey state
    return r

getProjectIconsFolder  :: IO FilePath
getProjectIconsFolder = (</> "projectIcons") <$> getAppDir

getIconPath :: Entity Project -> IO FilePath
getIconPath prj = (</> show (fromSqlKey32 prj) <> ".png") <$> getProjectIconsFolder

copyProjectIcons :: SqlBackend -> IO ()
copyProjectIcons sqlBackend = do
    getProjectIconsFolder >>= createDirectoryIfMissing True
    projects <- runSqlBackend sqlBackend readProjects
    let prjWithIcons = filter (not . BS.null . projectIcon . entityVal) projects
    forM_ prjWithIcons $ \prj -> do
        targetPath <- getIconPath prj
        BS.writeFile targetPath (projectIcon $ entityVal prj)

minSshTunnelPort :: Int
minSshTunnelPort = 1024

-- this function doesn't check if the port is
-- in fact free. The function doing all is
-- getNewSshTunnelPort.
getDbNextFreeSshTunnelPort :: SqlBackend -> IO Int
getDbNextFreeSshTunnelPort sqlBackend = do
    sshServerMaxPort <- (fmap unValue . listToMaybe) <$>
       runSqlBackend sqlBackend (select $ from $ \s -> do
       where_ (not_ $ isNothing (s ^. ServerSshTunnelPort))
       orderBy [desc (s ^. ServerSshTunnelPort)]
       limit 1
       return (s ^. ServerSshTunnelPort))
    return $ case sshServerMaxPort of
       Nothing   -> minSshTunnelPort
       Just port -> fromMaybe (error "null ssh port?") port + 1

getFirstFreePortAfter :: Int -> IO Int
getFirstFreePortAfter port = do
    when (port > 65535) $ error "Exhausted all the ports?!?"
    isFree <- isPortFree port
    if isFree
       then return port
       else getFirstFreePortAfter (port+1)

getNewSshTunnelPort :: SqlBackend -> IO Int
getNewSshTunnelPort sqlBackend = do
    firstCandidate <- getDbNextFreeSshTunnelPort sqlBackend
    getFirstFreePortAfter firstCandidate

-- when I want to copy something to the clipboard, I always have the text I want
-- to copy. The trouble is the feature that we support, to copy again to the
-- clipboard later.
-- I don't want to keep the string (which is most likely a password) in memory
-- the entire time. We are definitely vulnerable to memory dump attacks, but
-- I don't see the need to make it even worse. I'd still like to have passwords
-- in memory the shortest possible time.
-- So by remembering the entity type & entity ID, i can load again the password
-- should the user want to copy it again, without keeping it in memory.
getTextToCopyForEntity :: SqlBackend -> Text -> Int -> IO (Maybe Text)
getTextToCopyForEntity sqlBackend (readT -> entityType) entKey =
    case entityType of
        DatabaseEntityType        -> getP serverDatabasePassword
        ServerPoiEntityType       -> getP serverPointOfInterestPath
        ServerExtraUserEntityType -> getP serverExtraUserAccountPassword
        ServerWebsiteEntityType   -> getP serverWebsitePassword
        ServerEntityType          -> getP serverPassword
        _ -> error ("Don't know what field to copy for entity " ++ show entityType)
    where getP = readEntityField sqlBackend entKey

getEntityById :: (DefaultClass (Entity a), ToBackendKey SqlBackend a) =>
                 SqlBackend -> Int -> IO (Maybe (EntityRef a))
getEntityById sqlBackend entId = readEntityFromDb sqlBackend (toSqlKey32 entId)

getServerById :: SqlBackend -> Int -> IO (Maybe (EntityRef Server))
getServerById = getEntityById

getDatabaseById :: SqlBackend -> Int -> IO (Maybe (EntityRef ServerDatabase))
getDatabaseById = getEntityById

createProjectListState :: SqlBackend -> IO (ObjRef ProjectListState, SignalKey (IO ()))
createProjectListState sqlBackend = do
    changeKey <- newSignalKey
    rootClass <- newClass
        [
            defPropertySigRO' "projects" changeKey
                $ const $ sqlToQml sqlBackend readProjects,
            defPropertyConst' "projectIconsFolder" (const $ T.pack <$> getProjectIconsFolder),
            defMethod "addProject"          (addProject sqlBackend changeKey),
            defMethod "updateProject"       (updateProject sqlBackend changeKey),
            defMethod "deleteProjects"      (deleteProjects sqlBackend changeKey),
            defStatic "getServerById"       (getServerById sqlBackend),
            defStatic "getDatabaseById"     (getDatabaseById sqlBackend),
            defStatic "copyProjectIcons"    (copyProjectIcons sqlBackend),
            defStatic "getNewSshTunnelPort" (getNewSshTunnelPort sqlBackend),
            defStatic "getTextToCopyForEntity" (getTextToCopyForEntity sqlBackend)
        ]
    (,) <$> newObject rootClass () <*> return changeKey
