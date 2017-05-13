{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies, ConstraintKinds #-}
module CrudHelpers where

import Database.Esqueleto
import Graphics.QML
import Data.Maybe as M
import qualified Database.Persist as P
import Data.Traversable (traverse)
import Control.Exception
import Control.Error

import Model
import Util

-- helper used when adding an entity to DB.
addHelper :: (ToBackendKey SqlBackend record, SqlEntity s) =>
    SqlBackend -> Int -> (Key record -> s) -> IO ()
addHelper sqlBackend parentId entityGetter = do
    let pIdKey = toSqlKey $ fromIntegral parentId
    r <- try $ runSqlBackend sqlBackend $ P.insert $ entityGetter pIdKey
    case r of
        Left ex -> print (ex :: SomeException)
        Right _ -> return ()

updateHelper :: (DefaultClass (Entity b), SqlEntity b) =>
    SqlBackend -> EntityRef b
    -> [P.Update b] -> IO (EntityRef b)
updateHelper sqlBackend entityRef updateValues = do
    let idKey = entityKey $ fromObjRef entityRef
    runSqlBackend sqlBackend $ P.update idKey updateValues
    mEntity <- readEntityFromDb sqlBackend idKey
    return $ fromMaybe (error "update can't find back entity?") mEntity

readEntityFromDb :: (SqlEntity record, DefaultClass (Entity record)) =>
    SqlBackend -> Key record -> IO (Maybe (EntityRef record))
readEntityFromDb sqlBackend idKey = do
    entity <- runSqlBackend sqlBackend (P.get idKey)
    traverse (newObjectDC . Entity idKey) entity

-- meant to be called from HSQML. the t parameter
-- will hold the state of the object, but I don't care for it.
deleteHelper :: ToBackendKey SqlBackend a =>
                SqlBackend -> (Key a -> SqlPersistM ()) -> t -> [Int] -> IO (ObjRef (QmlResult ()))
deleteHelper sqlBackend deleter _ keys = wrapEx (mapM_ delKey keys)
    where
      wrapEx = liftQmlResult . fmap (fmapL textEx) . try
      delKey = runSqlBackend sqlBackend . deleter . toSqlKey32
