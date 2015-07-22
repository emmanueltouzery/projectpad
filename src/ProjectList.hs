{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module ProjectList where

import Control.Applicative
import Graphics.QML
import qualified Database.Persist as P
import Database.Esqueleto
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import Control.Monad
import System.FilePath.Posix
import System.Directory
import Data.Monoid

import Model
import CrudHelpers
import Util

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
            (text hasDev) (text hasUat) (text hasStage) (text hasProd))
    fireSignal changeKey state

updateProject :: SqlBackend -> SignalKey (IO ()) -> ObjRef ProjectListState
    -> ObjRef (Entity Project) -> Text -> Maybe Text -> Bool -> Bool
    -> Bool -> Bool -> IO (ObjRef (Entity Project))
updateProject sqlBackend changeKey state prj name iconPath hasDev hasUat hasStage hasProd = do
    let idKey = entityKey $ fromObjRef prj
    iconBytes <- readIcon iconPath
    runSqlBackend sqlBackend $ P.update idKey [ProjectName P.=. name,
        ProjectHasDev P.=. text hasDev, ProjectHasUat P.=. text hasUat,
        ProjectHasStage P.=. text hasStage, ProjectHasProd P.=. text hasProd,
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

createProjectListState :: SqlBackend -> IO (ObjRef ProjectListState, SignalKey (IO ()))
createProjectListState sqlBackend = do
    changeKey <- newSignalKey
    rootClass <- newClass
        [
            defPropertySigRO' "projects" changeKey
                $ const $ sqlToQml sqlBackend readProjects,
            defPropertyConst' "projectIconsFolder" (const $ T.pack <$> getProjectIconsFolder),
            defMethod "addProject"       (addProject sqlBackend changeKey),
            defMethod "updateProject"    (updateProject sqlBackend changeKey),
            defMethod "deleteProjects"   (deleteProjects sqlBackend changeKey),
            defStatic "copyProjectIcons" (copyProjectIcons sqlBackend)
        ]
    (,) <$> newObject rootClass () <*> return changeKey
