{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, ScopedTypeVariables #-}
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
-- QML files should be copied to install folder.
--
-- enforce unicity of project names

import Database.Persist.Sqlite
import Control.Applicative
import Graphics.QML
import Data.Typeable
import Database.Sqlite
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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

sqLiteDiscrimitator :: ByteString
sqLiteDiscrimitator = "SQLite format 3"

main :: IO ()
main = do
	sqlBackend <- getSqlBackend dbFileName
	displayApp sqlBackend

getSqlBackend :: Text -> IO SqlBackend
getSqlBackend t = do
	conn <- open t
	prepare conn "PRAGMA foreign_keys = ON;" >>= step
	-- to log the SQL queries as they're sent to the DB,
	-- add: import System.Log.FastLogger
	-- and use this callback:
	-- print . fromLogStr
	let logger _ _ _ = const $ return ()
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
isDbInitialized _ = isDbInitialized'

isDbInitialized' :: IO Bool
isDbInitialized' = do
	let path = T.unpack dbFileName
	fileExists <- doesFileExist path
	if not fileExists
		then return False
		else (>0) <$> withFile path ReadMode hFileSize 

-- might fail on windows if sqlite reserves exclusive
-- access to the file.
sanityCheckIsDbEncrypted :: IO Bool
sanityCheckIsDbEncrypted = do
	initialized <- isDbInitialized'
	let path = T.unpack dbFileName
	if (not initialized)
		then return True
		else do
			fileStart <- withFile path ReadMode
				(`BS.hGet` (BS.length sqLiteDiscrimitator))
			return $ fileStart /= sqLiteDiscrimitator

data UnlockResult = Ok
	| WrongPassword
	| DbNotEncrypted
	deriving (Read, Show)

setupPasswordAndUpgradeDb :: SqlBackend -> SignalKey (IO ())
	-> ObjRef ProjectListState -> ObjRef AppState -> Text -> IO Text
setupPasswordAndUpgradeDb sqlBackend changeKey state _ password = do
	encrypted <- sanityCheckIsDbEncrypted
	T.pack . show <$> if (not encrypted)
		then return DbNotEncrypted
		else do
			upgrade <- try $ runSqlBackend sqlBackend $ do
				rawExecute (T.concat ["PRAGMA key = '", password, "'"]) []
				upgradeSchema
			-- print details to STDOUT in case of failure because maybe
			-- it was the right password and the schema upgrade failed!
			case upgrade of
				(Left (x :: SomeException)) -> print x >> return WrongPassword
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
				$ return . serverViewState . fromObjRef,
			defMethod "openAssociatedFile" (\(_:: ObjRef AppState) path ->
				serializeEither <$> openAssociatedFile path)
		]
	rootContext <- AppState
		<$> return projectState
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
