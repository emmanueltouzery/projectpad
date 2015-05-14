{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
module CrudHelpers where

import Database.Esqueleto
import Control.Monad
import Graphics.QML
import Data.Maybe as M
import Control.Applicative
import qualified Database.Persist as P
import Data.Traversable (traverse)
import Control.Exception
import Data.Text (Text)
import Control.Error

import Model (runSqlBackend, toSqlKey32)
import Util

-- helper used when adding an entity to DB.
addHelper :: (ToBackendKey SqlBackend record, PersistEntity s, PersistEntityBackend s ~ SqlBackend) =>
    SqlBackend -> Int -> (Key record -> s) -> IO ()
addHelper sqlBackend parentId entityGetter = do
    let pIdKey = toSqlKey $ fromIntegral parentId
    void $ runSqlBackend sqlBackend $ P.insert $ entityGetter pIdKey

updateHelper :: (DefaultClass (Entity b), PersistEntity b,
    PersistEntityBackend b ~ SqlBackend) =>
    SqlBackend -> ObjRef (Entity b)
    -> [P.Update b] -> IO (ObjRef (Entity b))
updateHelper sqlBackend entityRef updateValues = do
    let idKey = entityKey $ fromObjRef entityRef
    runSqlBackend sqlBackend $ P.update idKey updateValues
    mEntity <- readEntityFromDb sqlBackend idKey
    return $ fromMaybe (error "update can't find back entity?") mEntity

readEntityFromDb :: (PersistEntity record, DefaultClass (Entity record),
    PersistEntityBackend record ~ SqlBackend) =>
    SqlBackend -> Key record -> IO (Maybe (ObjRef (Entity record)))
readEntityFromDb sqlBackend idKey = do
    entity <- runSqlBackend sqlBackend (P.get idKey)
    traverse (newObjectDC . Entity idKey) entity

-- meant to be called from HSQML. the t parameter
-- will hold the state of the object, but I don't care for it.
deleteHelper :: ToBackendKey SqlBackend a =>
                SqlBackend -> (Key a -> SqlPersistM ()) -> t -> [Int] -> IO [Text]
deleteHelper sqlBackend deleter _ keys = wrapEx (mapM_ delKey keys)
    where
      wrapEx = fmap (serializeEither' <$> fmapL textEx) . try
      delKey = runSqlBackend sqlBackend . deleter . toSqlKey32
