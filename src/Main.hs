{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, ViewPatterns, ScopedTypeVariables #-}
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
--
-- Probably don't need that ListChanged signal?
--
-- server type should be TEXT in model.
--
-- I have these caches in array properties in models
-- but maybe there's no point optimizing and I should
-- simply have functions returning the current state.
--
-- I think I don't need a factory pool but?

import Database.Persist.Sqlite
import Control.Applicative
import Graphics.QML
import Data.Typeable
import Database.Sqlite
import System.Log.FastLogger
import Data.Text (Text)
import qualified Data.Text as T
import System.IO
import System.Directory
import Control.Exception

import Model
import Schema
import ProjectList
import ProjectView
import ServerView

dbFileName :: Text
dbFileName = "projectpad.db"

main :: IO ()
main = do
	sqlBackend <- getSqlBackend dbFileName
	displayApp sqlBackend

getSqlBackend :: Text -> IO SqlBackend
getSqlBackend t = do
	conn <- open t
	prepare conn "PRAGMA foreign_keys = ON;" >>= step
	let logger = \_ _ _ -> print . fromLogStr
	wrapConnection conn logger

data AppState = AppState
	{
		projectListState :: ObjRef ProjectListState,
		projectViewState :: ObjRef ProjectViewState,
		serverViewState :: ObjRef ServerViewState
	} deriving Typeable

-- might fail on windows if sqlite reserves exclusive
-- access to the file.
isDbInitialized :: ObjRef AppState -> IO Bool
isDbInitialized _ = do
	let path = T.unpack dbFileName
	fileExists <- doesFileExist path
	if not fileExists
		then return False
		else (>0) <$> withFile path ReadMode hFileSize

data UnlockResult = Ok
	| WrongPassword
	deriving (Read, Show)

setupPasswordAndUpgradeDb :: SqlBackend -> SignalKey (IO ())
	-> ObjRef ProjectListState -> ObjRef AppState -> Text -> IO Text
setupPasswordAndUpgradeDb sqlBackend changeKey state _ password = T.pack . show <$> do
	upgrade <- try $ runSqlBackend sqlBackend $ do
		rawExecute (T.concat ["PRAGMA key = '", password, "'"]) []
		upgradeSchema
	case upgrade of
		(Left (_ :: SomeException)) -> return WrongPassword
		Right _ -> reReadProjects sqlBackend changeKey state >> return Ok

createContext :: SqlBackend -> IO (ObjRef AppState)
createContext sqlBackend = do
	(projectState, projectsChangeSignal) <- createProjectListState sqlBackend
	rootClass <- newClass
		[
			defMethod "isDbInitialized" isDbInitialized,
			defMethod "setupPasswordAndUpgradeDb" (setupPasswordAndUpgradeDb
				sqlBackend projectsChangeSignal projectState),
			defPropertyConst "projectListState"
				$ return . projectListState . fromObjRef,
			defPropertyConst "projectViewState"
				$ return . projectViewState . fromObjRef,
			defPropertyConst "serverViewState"
				$ return . serverViewState . fromObjRef
		]
	rootContext <- AppState
		<$> (return projectState)
		<*> createProjectViewState sqlBackend
		<*> createServerViewState sqlBackend
	newObject rootClass rootContext


displayApp :: SqlBackend -> IO ()
displayApp sqlBackend = do
	ctx <- createContext sqlBackend
	runEngineLoop defaultEngineConfig
		{
			initialDocument = fileDocument "qml/ProjectPad.qml",
			contextObject = Just $ anyObjRef ctx
		}
