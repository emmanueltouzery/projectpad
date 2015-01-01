{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
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

import Model
import Schema
import ProjectList
import ProjectView
import ServerView

main :: IO ()
main = do
	sqlBackend <- getSqlBackend "projectpad.db"
	runSqlBackend sqlBackend upgradeSchema
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

createContext :: SqlBackend -> IO (ObjRef AppState)
createContext sqlBackend = do
	rootClass <- newClass
		[
			defPropertyConst "projectListState"
				$ return . projectListState . fromObjRef,
			defPropertyConst "projectViewState"
				$ return . projectViewState . fromObjRef,
			defPropertyConst "serverViewState"
				$ return . serverViewState . fromObjRef
		]
	rootContext <- AppState
		<$> createProjectListState sqlBackend
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
