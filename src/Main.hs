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

import Database.Persist.Sqlite
import Control.Applicative
import Graphics.QML
import Data.Typeable
import Database.Sqlite
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import System.IO
import System.Directory
import Control.Exception
import System.FilePath.Posix
import Control.Monad
import System.Log.FastLogger
import System.Environment
import Data.List
import Data.Char
import Control.Error

import Paths_projectpad

import Model
import Schema
import System
import ProjectList
import ProjectView
import ServerView
import Search
import Util
import Notes

dbFileName :: String
dbFileName = "projectpad.db"

projectPadFolder :: String
projectPadFolder = ".projectpad"

sqLiteDiscrimitator :: ByteString
sqLiteDiscrimitator = "SQLite format 3"

main :: IO ()
main = do
    logQueriesToStdout <- elem "--printQueries" <$> getArgs
    appDir <- getAppDir
    createDirectoryIfMissing True appDir
    sqlBackend <- getSqlBackend logQueriesToStdout (appDir </> dbFileName)
    displayApp sqlBackend

getAppDir :: IO String
getAppDir = (</> projectPadFolder) <$> getHomeDirectory

getDbPath :: IO String
getDbPath = do
    appDir <- getAppDir
    return (appDir </> dbFileName)

getSqlBackend :: Bool -> String -> IO SqlBackend
getSqlBackend logQueriesToStdout (T.pack -> t) = do
    conn <- open t
    prepare conn "PRAGMA foreign_keys = ON;" >>= step
    let logger _ _ _ = if logQueriesToStdout
        then putStrLn . getLogQuery
        else const $ return ()
    wrapConnection conn logger

-- hiding the PRAGMA calls because of the first PRAGMA
-- call which contains the password to the database...
-- don't want to print that one in the logs.
getLogQuery :: LogStr -> String
getLogQuery logStr = if "PRAGMA" `isPrefixOf` normalizedQuery
                     then "<PRAGMA call>"
                     else origQuery
    where
      origQuery = BS8.unpack (fromLogStr logStr)
      normalizedQuery = toUpper <$> dropWhile (`elem` "\" \t") origQuery

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
    if not initialized
        then return True
        else do
            fileStart <- withFile path ReadMode
                (`BS.hGet` BS.length sqLiteDiscrimitator)
            return $ fileStart /= sqLiteDiscrimitator

data UnlockResult = Ok
    | WrongPassword
    | DbNotEncrypted
    deriving (Read, Show)

setupPasswordAndUpgradeDb :: SqlBackend -> ObjRef AppState -> Text -> Text -> IO Text
setupPasswordAndUpgradeDb sqlBackend _ password newPassword = do
    encrypted <- sanityCheckIsDbEncrypted
    T.pack . show <$> if not encrypted
        then return DbNotEncrypted
        else do
            upgrade <- try $ runSqlBackend sqlBackend $ do
                rawExecute (T.concat ["PRAGMA key = '", password, "'"]) []
                when (T.length newPassword > 0) $
                    rawExecute (T.concat ["PRAGMA rekey = '", newPassword, "'"]) []
                upgradeSchema
            -- print details to STDOUT in case of failure because maybe
            -- it was the right password and the schema upgrade failed!
            case upgrade of
                (Left (x :: SomeException)) -> print x >> return WrongPassword
                Right _ -> return Ok

createContext :: SqlBackend -> IO (ObjRef AppState)
createContext sqlBackend = do
    projectState <- fst <$> createProjectListState sqlBackend
    rootClass <- newClass
        [
            defMethod' "isDbInitialized" $ const isDbInitialized,
            defMethod "setupPasswordAndUpgradeDb" (setupPasswordAndUpgradeDb sqlBackend),
            defPropertyConst "projectListState"
                $ return . projectListState . fromObjRef,
            defPropertyConst "projectViewState"
                $ return . projectViewState . fromObjRef,
            defPropertyConst "serverViewState"
                $ return . serverViewState . fromObjRef,
            defMethod' "search" (const $ searchText sqlBackend),
            defMethod' "openAssociatedFile" (\_ path ->
                serializeEither' <$> openAssociatedFile path),
            defStatic "noteTextToHtml" $ liftQmlResult1 (return . fmapL T.pack
                 . fmap noteDocumentToHtmlText . parseNoteDocument)
        ]
    rootContext <- AppState
        <$> return projectState
        <*> createProjectViewState sqlBackend
        <*> createServerViewState sqlBackend
    newObject rootClass rootContext


displayApp :: SqlBackend -> IO ()
displayApp sqlBackend = do
    ctx <- createContext sqlBackend
    mainQml <- Paths_projectpad.getDataFileName "qml/ProjectPad.qml"
    runEngineLoop defaultEngineConfig
        {
            initialDocument = fileDocument mainQml,
            contextObject = Just $ anyObjRef ctx
        }
