{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ViewPatterns, TypeFamilies #-}
module ChildEntityHolder where

import Control.Concurrent.MVar
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

swapMVar_ :: MVar a -> a -> IO ()
swapMVar_ a b = void (swapMVar a b)

class DynParentHolder a where
    dynParentId :: a -> MVar (Maybe Int)

getCurParentId :: DynParentHolder a => ObjRef a -> IO Int
getCurParentId (fromObjRef -> state) = fromMaybe (error "No current parent ID!")
    <$> readMVar (dynParentId state)

-- read children from the cache for a certain parent id.
-- if the cache is for another parent id, reset the cache
-- for the new parent id, and refill it before returning the value.
getChildren :: (DynParentHolder a, DefaultClass (Entity b)) =>
    SqlBackend -> (Int -> SqlPersistM [Entity b]) -> ObjRef a -> Int -> IO [ObjRef (Entity b)]
getChildren sqlBackend readChildren _state parentId = do
    let state = fromObjRef _state
    pId <- readMVar (dynParentId state)
    when (pId /= Just parentId) $
        swapMVar_ (dynParentId state) (Just parentId)
    mapM newObjectDC =<< runSqlBackend sqlBackend (readChildren parentId)

-- helper used when adding an entity to DB.
addHelper :: (DynParentHolder a,
    ToBackendKey SqlBackend record, PersistEntity s, PersistEntityBackend s ~ SqlBackend) =>
    SqlBackend -> ObjRef a -> (Key record -> s) -> IO ()
addHelper sqlBackend stateRef entityGetter = do
    pIdKey <- toSqlKey . fromIntegral <$> getCurParentId stateRef
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
