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
import Database.Sqlite as Sqlite
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
import Control.Concurrent.MVar

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

sqLiteDiscrimitator :: ByteString
sqLiteDiscrimitator = "SQLite format 3"

main :: IO ()
main = do
    logQueriesToStdout <- elem "--printQueries" <$> getArgs
    appDir <- getAppDir
    createDirectoryIfMissing True appDir
    let dbFullPath = T.pack $ appDir </> dbFileName
    displayApp dbFullPath logQueriesToStdout

getDbPath :: IO String
getDbPath = do
    appDir <- getAppDir
    return (appDir </> dbFileName)

-- hiding the PRAGMA calls because of the first PRAGMA
-- call which contains the password to the database...
-- don't want to print that one in the logs.
getLogQuery :: LogStr -> String
getLogQuery logStr = if "PRAGMA" `isPrefixOf` normalizedQuery
                     then "<PRAGMA call>"
                     else origQuery
    where
      origQuery = BS8.unpack (fromLogStr logStr)
      normalizedQuery = toUpper <$> dropWhile (`elem` ['"', ' ', '\t']) origQuery

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


openAndUnlockDb :: Text -> Text -> Text -> IO Sqlite.Connection
openAndUnlockDb dbFullPath password newPassword = do
    conn <- open dbFullPath
    prepare conn "PRAGMA foreign_keys = ON;" >>= step
    prepare conn (T.concat ["PRAGMA key = '", password, "'"]) >>= step
    when (T.length newPassword > 0) $
        void $ prepare conn (T.concat ["PRAGMA rekey = '", newPassword, "'"]) >>= step
    return conn

disableWal :: Sqlite.Connection -> IO ()
disableWal conn = do
    -- i didn't manage easily to stop persistent from enabling
    -- the WAL but disable it manually after they do it:
    -- https://github.com/yesodweb/persistent/issues/363#issuecomment-114239523
    turnOnWal <- prepare conn "PRAGMA journal_mode=DELETE;"
    _ <- step turnOnWal
    reset conn turnOnWal
    finalize turnOnWal

getSqlBackend :: Text -> Bool -> Text -> Text -> IO SqlBackend
getSqlBackend dbFullPath logQueriesToStdout password newPassword = do
    conn <- openAndUnlockDb dbFullPath password newPassword
    let logger _ _ _ = if logQueriesToStdout
        then putStrLn . getLogQuery
        else const $ return ()
    sqlBackend <- wrapConnection conn logger
    disableWal conn
    return sqlBackend

setupPasswordAndUpgradeDb :: Text -> Bool -> ObjRef LoginState -> Text -> Text -> IO Text
setupPasswordAndUpgradeDb dbFullPath logQueriesToStdout (fromObjRef -> loginState) password newPassword = do
    encrypted <- sanityCheckIsDbEncrypted
    T.pack . show <$> if not encrypted
        then return DbNotEncrypted
        else do
            setupResult <- try $ do
                sqlBackend <- getSqlBackend dbFullPath
                              logQueriesToStdout password newPassword
                runSqlBackend sqlBackend upgradeSchema
                return sqlBackend
            -- print details to STDOUT in case of failure because maybe
            -- it was the right password and the schema upgrade failed!
            case setupResult of
                (Left (x :: SomeException)) -> print x >> return WrongPassword
                Right sqlBackend -> setupContext loginState sqlBackend >> return Ok

data AppState = AppState
    {
        projectListState :: ObjRef ProjectListState,
        projectViewState :: ObjRef ProjectViewState,
        serverViewState  :: ObjRef ServerViewState
    } deriving Typeable

setupContext :: LoginState -> SqlBackend -> IO ()
setupContext loginState sqlBackend = do
    projectState <- fst <$> createProjectListState sqlBackend
    rootClass <- newClass
        [
            defPropertyConst "projectListState"
                $ return . projectListState . fromObjRef,
            defPropertyConst "projectViewState"
                $ return . projectViewState . fromObjRef,
            defPropertyConst "serverViewState"
                $ return . serverViewState . fromObjRef,
            defStatic "search" (\e t -> searchText sqlBackend (readT e) t),
            defMethod' "openAssociatedFile" (\_ path ->
                liftQmlResult $ openAssociatedFile path),
            defStatic "noteTextToHtml" $ liftQmlResult1 (return . fmapL T.pack
                 . fmap noteDocumentToHtmlText . parseNoteDocument)
        ]
    rootContext <- AppState
        <$> return projectState
        <*> createProjectViewState sqlBackend
        <*> createServerViewState sqlBackend
    appCtx <- newObject rootClass rootContext
    void $ swapMVar (appContext loginState) (Just appCtx)

data LoginState = LoginState
    {
        appContext :: MVar (Maybe (ObjRef AppState))
    } deriving Typeable

getAppState :: ObjRef LoginState -> IO (Maybe (ObjRef AppState))
getAppState (fromObjRef -> loginState) = readMVar (appContext loginState)

createLoginContext :: Text -> Bool -> IO (ObjRef LoginState)
createLoginContext dbFullPath logQueriesToStdout = do
    loginClass <- newClass
        [
            defMethod' "isDbInitialized" $ const isDbInitialized,
            defMethod  "setupPasswordAndUpgradeDb"
                (setupPasswordAndUpgradeDb dbFullPath logQueriesToStdout),
            defMethod  "getAppState" getAppState
        ]
    newObject loginClass =<< LoginState <$> newMVar Nothing

displayApp :: Text -> Bool -> IO ()
displayApp dbFullPath logQueriesToStdout = do
    ctx <- createLoginContext dbFullPath logQueriesToStdout
    mainQml <- Paths_projectpad.getDataFileName "qml/ProjectPad.qml"
    runEngineLoop defaultEngineConfig
        {
            initialDocument = fileDocument mainQml,
            contextObject = Just $ anyObjRef ctx
        }
