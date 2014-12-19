{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
module Main where

-- the DB init should be done in a try block
-- (if i don't have the rights and so on)
--
-- create a unsafePerformIO error call that displays
-- a message box, use it instead of error everywhere
-- in case of terminal failure
--
-- create db file in ~/.projectpad

import Database.Persist.Sqlite
import Database.Esqueleto
import Control.Monad.Trans
import Control.Applicative
import Graphics.QML
import Control.Concurrent.MVar
import Data.Typeable

import Model
import Schema

main :: IO ()
main = runSqlite "projectpad.db" $ do
	upgradeSchema
	--P.insert (Project "LTA" "")
	p <- select $ from $ \p -> do
		orderBy [asc (p ^. ProjectName)]
		return p
	liftIO $ print $ entityVal <$> p

	liftIO displayApp

-- Signals
data ListChanged deriving Typeable

instance SignalKeyClass ListChanged where
    type SignalParams ListChanged = IO ()

data ProjectScreenState = ProjectScreenState
	{
		projects :: MVar [ObjRef Project]
	} deriving Typeable

instance DefaultClass ProjectScreenState where
	classMembers = 
		[
			defPropertySigRO "projects" (Proxy :: Proxy ListChanged)
				$ readMVar . projects . fromObjRef
		]

displayApp :: IO ()
displayApp = do
	c1 <- newObjectDC $ Project "C1" ""
	c2 <- newObjectDC $ Project "C2" ""
	c3 <- newObjectDC $ Project "C3" ""
	projectScreenState <- ProjectScreenState <$> newMVar [c1, c2, c3]
	ctx <- newObjectDC projectScreenState

	runEngineLoop defaultEngineConfig
		{
			initialDocument = fileDocument "projectpad.qml",
			contextObject = Just $ anyObjRef ctx
		}
