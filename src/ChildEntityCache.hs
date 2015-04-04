{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ViewPatterns, TypeFamilies #-}
module ChildEntityCache where

import Control.Concurrent.MVar
import Database.Esqueleto
import Control.Monad
import Graphics.QML
import Data.Maybe as M
import Control.Applicative
import qualified Database.Persist as P
import Data.List

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
	clearAllChildrenCaches :: a -> IO ()

class DynParentHolder a => CacheHolder b a where
	cacheChildren :: a -> EntityListCache b

getCurParentId :: DynParentHolder a => ObjRef a -> IO Int
getCurParentId (fromObjRef -> state) = fromMaybe (error "No current parent ID!")
	<$> readMVar (dynParentId state)

clearCache :: DynParentHolder a => ObjRef a -> IO ()
clearCache (fromObjRef -> state) = do
	swapMVar_ (dynParentId state) Nothing
	clearAllChildrenCaches state

-- the fact I have this function at all is probably
-- the sign that I should drop this cache completely.
hasCache :: DynParentHolder a => ObjRef a -> IO Bool
hasCache (fromObjRef -> state) = isJust <$> readMVar (dynParentId state)

updateCache :: (CacheHolder b a, DefaultClass (Entity b)) =>
       ObjRef a -> [Entity b] -> IO [ObjRef (Entity b)]
updateCache state_ newChildren = do
	newQmlChildren <- mapM newObjectDC newChildren
	swapMVar_ (cacheChildren $ fromObjRef state_) (Just newQmlChildren)
	return newQmlChildren

updateCacheQuery :: (CacheHolder b a, DefaultClass (Entity b)) =>
	SqlBackend -> ObjRef a -> (Int -> SqlPersistM [Entity b]) -> IO ()
updateCacheQuery sqlBackend stateRef reader = do
	pId <- getCurParentId stateRef
	newData <- runSqlBackend sqlBackend (reader pId)
	updateCache stateRef newData
	return ()

-- read children from the cache for a certain parent id.
-- if the cache is for another parent id, reset the cache
-- for the new parent id, and refill it before returning the value.
getChildren :: (CacheHolder b a, DefaultClass (Entity b)) =>
	SqlBackend -> (Int -> SqlPersistM [Entity b]) -> ObjRef a -> Int -> IO [ObjRef (Entity b)]
getChildren sqlBackend readChildren _state parentId = do
	let state = fromObjRef _state
	pId <- readMVar (dynParentId state)
	when (pId /= Just parentId) $ do
		swapMVar_ (dynParentId state) (Just parentId)
		clearAllChildrenCaches state
	cachedData <- readMVar (cacheChildren state)
	case cachedData of
		Just x -> return x
		Nothing -> do
			children <- runSqlBackend sqlBackend (readChildren parentId)
			updateCache _state children

-- helper used when adding an entity to DB.
addHelper :: (CacheHolder b a, DefaultClass (Entity b),
	ToBackendKey SqlBackend record, PersistEntity s, PersistEntityBackend s ~ SqlBackend) =>
	SqlBackend -> ObjRef a -> (Int -> SqlPersistM [Entity b]) -> (Key record -> s) -> IO ()
addHelper sqlBackend stateRef reader entityGetter = do
	pIdKey <- toSqlKey . fromIntegral <$> getCurParentId stateRef
	runSqlBackend sqlBackend $ P.insert $ entityGetter pIdKey
	updateCacheQuery sqlBackend stateRef reader

updateHelper :: (CacheHolder b a, DefaultClass (Entity b), PersistEntity b,
	PersistEntityBackend b ~ SqlBackend) =>
	SqlBackend -> ObjRef a -> ObjRef (Entity b) -> (Int -> SqlPersistM [Entity b])
	-> (a -> EntityListCache b) -> [P.Update b] -> IO (ObjRef (Entity b))
updateHelper sqlBackend stateRef entityRef reader stateReader updateValues = do
	let idKey = entityKey $ fromObjRef entityRef
	runSqlBackend sqlBackend $ P.update idKey updateValues
	cache <- hasCache stateRef
	mEntity <- if cache
		then handleUpdateCache sqlBackend stateRef reader stateReader idKey
		else readEntityFromDb sqlBackend idKey
	return $ fromMaybe (error "update can't find back entity?") mEntity

readEntityFromDb :: (PersistEntity record, DefaultClass (Entity record),
	PersistEntityBackend record ~ SqlBackend) =>
	SqlBackend -> Key record -> IO (Maybe (ObjRef (Entity record)))
readEntityFromDb sqlBackend idKey = do
	entity <- runSqlBackend sqlBackend (P.get idKey)
	case entity of
		Nothing -> return Nothing
		Just e -> Just <$> newObjectDC (Entity idKey e)

handleUpdateCache :: (CacheHolder b tt, DefaultClass (Entity b), Eq (Key record)) =>
	SqlBackend -> ObjRef tt -> (Int -> SqlPersistM [Entity b])
	-> (tt -> MVar (Maybe [ObjRef (Entity record)]))
	-> Key record -> IO (Maybe (ObjRef (Entity record)))
handleUpdateCache sqlBackend stateRef reader stateReader idKey = do
	updateCacheQuery sqlBackend stateRef reader
	mEntityList <- readMVar (stateReader (fromObjRef stateRef))
	return $ find ((== idKey) . entityKey . fromObjRef) =<< mEntityList

convertKey :: (ToBackendKey SqlBackend a) => Int -> Key a
convertKey = toSqlKey . fromIntegral

deleteHelper :: (CacheHolder b a, DefaultClass (Entity b),
		PersistEntity b, PersistEntityBackend b ~ SqlBackend) =>
        	(Int -> Key b) -> (Int -> SqlPersistM [Entity b])
		-> SqlBackend -> ObjRef a -> [Int] -> IO ()
deleteHelper keyConverter reader sqlBackend stateRef serverIds = do
	let keys = fmap keyConverter serverIds
	mapM_ (runSqlBackend sqlBackend . P.delete) keys
	cache <- hasCache stateRef
	when cache $ updateCacheQuery sqlBackend stateRef reader
