{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ViewPatterns #-}
module ChildEntityCache where

import Control.Concurrent.MVar
import Database.Esqueleto
import Control.Monad
import Graphics.QML
import Data.Maybe
import Control.Applicative

swapMVar_ :: MVar a -> a -> IO ()
swapMVar_ a b = swapMVar a b >> return ()

class DynParentHolder a where
	dynParentId :: a -> MVar (Maybe Int)
	clearAllChildrenCaches :: a -> IO ()

class DynParentHolder a => CacheHolder b a where
	cacheChildren :: a -> MVar [ObjRef (Entity b)]

getCurParentId :: DynParentHolder a => ObjRef a -> IO Int
getCurParentId (fromObjRef -> state) = fromMaybe (error "No current parent ID!")
	<$> readMVar (dynParentId state)

updateCache :: (CacheHolder b a, DefaultClass (Entity b)) =>
	ObjRef a -> [Entity b] -> Int -> IO ()
updateCache state_ newChildren parentId = do
	let state = fromObjRef state_
	newQmlChildren <- mapM newObjectDC newChildren
	swapMVar_ (dynParentId state) (Just parentId)
	clearAllChildrenCaches state
	-- update this cache
	swapMVar_ (cacheChildren state) newQmlChildren
	-- maybe i need to fire a signal for hsqml so it updates the objref?

getChildren :: (CacheHolder b a, DefaultClass (Entity b)) =>
	(Int -> IO [Entity b]) -> ObjRef a -> Int -> IO [ObjRef (Entity b)]
getChildren readChildren _state parentId = do
	let state = fromObjRef _state
	pId <- readMVar (dynParentId state)
	when (not $ pId == Just parentId) $ do
		newChildren <- readChildren parentId
		updateCache _state newChildren parentId
	readMVar $ cacheChildren state
