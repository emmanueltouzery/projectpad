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
updateHelper sqlBackend stateRef entityRef reader stateReader updateValues= do
	let idKey = entityKey $ fromObjRef entityRef
	runSqlBackend sqlBackend $ P.update idKey updateValues
	updateCacheQuery sqlBackend stateRef reader
	newEntityList <- fromMaybe (error "No entities after update?")
		<$> readMVar (stateReader (fromObjRef stateRef))
	let mUpdatedEntity = find ((== idKey) . entityKey . fromObjRef) newEntityList
	return $ fromMaybe (error "Can't find entity after update?") mUpdatedEntity

convertKey :: (ToBackendKey SqlBackend a) => Int -> Key a
convertKey = toSqlKey . fromIntegral

deleteHelper :: (CacheHolder b a, DefaultClass (Entity b),
		PersistEntity b, PersistEntityBackend b ~ SqlBackend) =>
        	(Int -> Key b) -> (Int -> SqlPersistM [Entity b])
		-> SqlBackend -> ObjRef a -> [Int] -> IO ()
deleteHelper keyConverter reader sqlBackend stateRef serverIds = do
	let keys = fmap keyConverter serverIds
	mapM_ (runSqlBackend sqlBackend . P.delete) keys
	updateCacheQuery sqlBackend stateRef reader
