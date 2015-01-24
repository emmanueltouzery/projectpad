{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, ScopedTypeVariables, ViewPatterns #-}
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
-- QML files should be copied to install folder.

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
import System.FilePath.Posix

import Model
import Schema
import System
import ProjectList
import ProjectView
import ServerView

dbFileName :: String
dbFileName = "projectpad.db"

projectPadFolder :: String
projectPadFolder = ".projectpad"

sqLiteDiscrimitator :: ByteString
sqLiteDiscrimitator = "SQLite format 3"

main :: IO ()
main = do
	appDir <- getAppDir
	createDirectoryIfMissing True appDir
	sqlBackend <- getSqlBackend (appDir </> dbFileName)
	displayApp sqlBackend

getAppDir :: IO String
getAppDir = (</> projectPadFolder) <$> getHomeDirectory

getDbPath :: IO String
getDbPath = do
	appDir <- getAppDir
	return (appDir </> dbFileName)

getSqlBackend :: String -> IO SqlBackend
getSqlBackend (T.pack -> t) = do
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
isDbInitialized :: IO Bool
isDbInitialized = do
	path <- getDbPath
	fileExists <- doesFileExist path
	if not fileExists
		then return False
		else (>0) <$> withFile path ReadMode hFileSize 

-- might fail on windows if sqlite reserves exclusive
-- access to the file.
sanityCheckIsDbEncrypted :: IO Bool
sanityCheckIsDbEncrypted = do
	initialized <- isDbInitialized
	path <- getDbPath
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
			defMethod' "isDbInitialized" $ const isDbInitialized,
			defMethod "setupPasswordAndUpgradeDb" (setupPasswordAndUpgradeDb
				sqlBackend projectsChangeSignal projectState),
			defPropertyConst "projectListState"
				$ return . projectListState . fromObjRef,
			defPropertyConst "projectViewState"
				$ return . projectViewState . fromObjRef,
			defPropertyConst "serverViewState"
				$ return . serverViewState . fromObjRef,
			defMethod' "openAssociatedFile" $ (\_ path ->
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
