{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
module ProjectList where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import qualified Database.Persist as P
import Data.Typeable
import Database.Esqueleto
import Data.Text (Text)
import Data.List
import Data.Maybe
import Control.Monad

import Model
import ChildEntityCache

type ProjectListState = ()

readProjects :: SqlPersistM [Entity Project]
readProjects = select $ from $ \p -> do
    orderBy [asc (p ^. ProjectName)]
    return p

addProject :: SqlBackend -> SignalKey (IO ()) -> ObjRef ProjectListState
           -> Text -> Bool -> Bool -> Bool -> Bool -> IO ()
addProject sqlBackend changeKey state txt hasDev hasUat hasStage hasProd = do
    -- a project must have at least one environment. Refuse to update otherwise.
    when (hasDev || hasUat || hasStage || hasProd) $ void $
        runSqlBackend sqlBackend $ P.insert (Project txt ""
            (text hasDev) (text hasUat) (text hasStage) (text hasProd))
    fireSignal changeKey state

updateProject :: SqlBackend -> SignalKey (IO ()) -> ObjRef ProjectListState
    -> ObjRef (Entity Project) -> Text -> Bool -> Bool
    -> Bool -> Bool -> IO (ObjRef (Entity Project))
updateProject sqlBackend changeKey state
    prj name hasDev hasUat
    hasStage hasProd = do
    let idKey = entityKey $ fromObjRef prj
    runSqlBackend sqlBackend $ P.update idKey [ProjectName P.=. name,
        ProjectHasDev P.=. text hasDev, ProjectHasUat P.=. text hasUat,
        ProjectHasStage P.=. text hasStage, ProjectHasProd P.=. text hasProd]
    fireSignal changeKey state
    newProjectList <- runSqlBackend sqlBackend readProjects
    let mUpdatedProjectEntity = find ((== idKey) . entityKey) newProjectList
    newObjectDC $ fromMaybe (error "Can't find project after update?") mUpdatedProjectEntity

deleteProjects :: SqlBackend -> SignalKey (IO ()) -> ObjRef ProjectListState -> [Int] -> IO ()
deleteProjects sqlBackend changeKey state projectIds = do
    let keys = fmap convertKey projectIds :: [Key Project]
    mapM_ (runSqlBackend sqlBackend . P.delete) keys
    fireSignal changeKey state

createProjectListState :: SqlBackend -> IO (ObjRef ProjectListState, SignalKey (IO ()))
createProjectListState sqlBackend = do
    changeKey <- newSignalKey
    rootClass <- newClass
        [
            defPropertySigRO' "projects" changeKey
                $ const $ mapM newObjectDC =<< runSqlBackend sqlBackend readProjects,
            defMethod "addProject" (addProject sqlBackend changeKey),
            defMethod "updateProject" (updateProject sqlBackend changeKey),
            defMethod "deleteProjects" (deleteProjects sqlBackend changeKey)
        ]
    (,) <$> newObject rootClass () <*> return changeKey
