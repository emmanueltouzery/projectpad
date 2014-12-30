{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ViewPatterns #-}
module ChildEntityCache where

import Control.Concurrent.MVar
import Database.Esqueleto
import Control.Monad
import Graphics.QML
import Data.Maybe as M
import Control.Applicative

swapMVar_ :: MVar a -> a -> IO ()
swapMVar_ a b = swapMVar a b >> return ()

class DynParentHolder a where
	dynParentId :: a -> MVar (Maybe Int)
	clearAllChildrenCaches :: a -> IO ()

class DynParentHolder a => CacheHolder b a where
	cacheChildren :: a -> MVar (Maybe [ObjRef (Entity b)])

getCurParentId :: DynParentHolder a => ObjRef a -> IO Int
getCurParentId (fromObjRef -> state) = fromMaybe (error "No current parent ID!")
	<$> readMVar (dynParentId state)

updateCache :: (CacheHolder b a, DefaultClass (Entity b)) =>
       ObjRef a -> [Entity b] -> IO [ObjRef (Entity b)]
updateCache state_ newChildren = do
	newQmlChildren <- mapM newObjectDC newChildren
	swapMVar_ (cacheChildren $ fromObjRef state_) (Just newQmlChildren)
	return newQmlChildren

-- read children from the cache for a certain parent id.
-- if the cache is for another parent id, reset the cache
-- for the new parent id, and refill it before returning the value.
getChildren :: (CacheHolder b a, DefaultClass (Entity b)) =>
	(Int -> IO [Entity b]) -> ObjRef a -> Int -> IO [ObjRef (Entity b)]
getChildren readChildren _state parentId = do
	let state = fromObjRef _state
	pId <- readMVar (dynParentId state)
	when (not $ pId == Just parentId) $ do
		swapMVar_ (dynParentId state) (Just parentId)
		clearAllChildrenCaches state
	cachedData <- readMVar (cacheChildren state)
	case cachedData of
		Just x -> return x
		Nothing -> readChildren parentId >>= updateCache _state
