{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ViewPatterns, TypeFamilies #-}
module ChildEntityCache where

import Control.Concurrent.MVar
import Database.Esqueleto
import Control.Monad
import Graphics.QML
import Data.Maybe as M
import Control.Applicative
import qualified Database.Persist as P

import Model (runSqlBackend)

-- I came up with this cache to avoid creating too many
-- QML objects backing a single DB object. But maybe it's
-- just complication for no reason. Especially since I now
-- sidestep the cache when using search...

type EntityListCache a = MVar (Maybe [ObjRef (Entity a)])

swapMVar_ :: MVar a -> a -> IO ()
swapMVar_ a b = void (swapMVar a b)

class DynParentHolder a where
    dynParentId :: a -> MVar (Maybe Int)

getCurParentId :: DynParentHolder a => ObjRef a -> IO Int
getCurParentId (fromObjRef -> state) = fromMaybe (error "No current parent ID!")
    <$> readMVar (dynParentId state)

clearCache :: DynParentHolder a => ObjRef a -> IO ()
clearCache (fromObjRef -> state) = do
    swapMVar_ (dynParentId state) Nothing

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
addHelper :: (DynParentHolder a, DefaultClass (Entity b),
    ToBackendKey SqlBackend record, PersistEntity s, PersistEntityBackend s ~ SqlBackend) =>
    SqlBackend -> ObjRef a -> (Int -> SqlPersistM [Entity b]) -> (Key record -> s) -> IO ()
addHelper sqlBackend stateRef reader entityGetter = do
    pIdKey <- toSqlKey . fromIntegral <$> getCurParentId stateRef
    void $ runSqlBackend sqlBackend $ P.insert $ entityGetter pIdKey

updateHelper :: (DynParentHolder a, DefaultClass (Entity b), PersistEntity b,
    PersistEntityBackend b ~ SqlBackend) =>
    SqlBackend -> ObjRef a -> ObjRef (Entity b)
    -> [P.Update b] -> IO (ObjRef (Entity b))
updateHelper sqlBackend stateRef entityRef updateValues = do
    let idKey = entityKey $ fromObjRef entityRef
    runSqlBackend sqlBackend $ P.update idKey updateValues
    mEntity <- readEntityFromDb sqlBackend idKey
    return $ fromMaybe (error "update can't find back entity?") mEntity

readEntityFromDb :: (PersistEntity record, DefaultClass (Entity record),
    PersistEntityBackend record ~ SqlBackend) =>
    SqlBackend -> Key record -> IO (Maybe (ObjRef (Entity record)))
readEntityFromDb sqlBackend idKey = do
    entity <- runSqlBackend sqlBackend (P.get idKey)
    case entity of
        Nothing -> return Nothing
        Just e -> Just <$> newObjectDC (Entity idKey e)

convertKey :: (ToBackendKey SqlBackend a) => Int -> Key a
convertKey = toSqlKey . fromIntegral

deleteHelper :: (DynParentHolder a, DefaultClass (Entity b),
        PersistEntity b, PersistEntityBackend b ~ SqlBackend) =>
            (Int -> Key b) -> (Int -> SqlPersistM [Entity b])
        -> SqlBackend -> ObjRef a -> [Int] -> IO ()
deleteHelper keyConverter reader sqlBackend stateRef serverIds = do
    let keys = fmap keyConverter serverIds
    void $ mapM_ (runSqlBackend sqlBackend . P.delete) keys
