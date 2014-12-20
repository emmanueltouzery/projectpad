{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
module Main where

-- the DB init should be done in a try block
-- (if i don't have the rights and so on)
--
-- create a unsafePerformIO error call that displays
-- a message box, use it instead of error everywhere
-- in case of terminal failure
-- proper logger for the SQL errors that goes all
-- the way to the GUI.
--
-- create db file in ~/.projectpad
--
-- enforce unicity of project names

import Database.Persist.Sqlite
import Database.Esqueleto
import Control.Monad.Trans
import Control.Applicative
import Graphics.QML
import Control.Concurrent.MVar
import Data.Typeable
import Data.Text (Text)
import qualified Database.Persist as P
import Database.Sqlite
import System.Log.FastLogger

import Model
import Schema

main :: IO ()
main = do
	conn <- open "projectpad.db"
	let logger = \_ _ _ -> print . fromLogStr
	sqlBackend <- wrapConnection conn logger
	prj <- runSqlBackend sqlBackend $ do
		upgradeSchema

		select $ from $ \p -> do
			orderBy [asc (p ^. ProjectName)]
			return p
	mapM newObjectDC prj >>= displayApp sqlBackend

runSqlBackend :: SqlBackend -> SqlPersistM a -> IO a
runSqlBackend = flip runSqlPersistM

-- Signals
data ListChanged deriving Typeable

instance SignalKeyClass ListChanged where
    type SignalParams ListChanged = IO ()

data ProjectScreenState = ProjectScreenState
	{
		projects :: MVar [ObjRef (Entity Project)]
	} deriving Typeable

createContext :: SqlBackend -> ProjectScreenState -> IO (ObjRef ProjectScreenState)
createContext sqlBackend state = do
	rootClass <- newClass
		[
			defPropertySigRO "projects" (Proxy :: Proxy ListChanged)
				$ readMVar . projects . fromObjRef,
			defMethod "addProject" (addProject sqlBackend)
		]
	newObject rootClass state

addProject :: SqlBackend -> ObjRef ProjectScreenState -> Text -> IO ()
addProject sqlBackend state text = runSqlBackend sqlBackend $ do
	P.insert (Project text "")
	liftIO $ fireSignal (Proxy :: Proxy ListChanged) state

displayApp :: SqlBackend -> [ObjRef (Entity Project)] -> IO ()
displayApp sqlBackend prj = do
	projectScreenState <- ProjectScreenState <$> newMVar prj

	ctx <- createContext sqlBackend projectScreenState

	runEngineLoop defaultEngineConfig
		{
			initialDocument = fileDocument "projectpad.qml",
			contextObject = Just $ anyObjRef ctx
		}
