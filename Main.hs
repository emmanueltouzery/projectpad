{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Control.Error
import Control.Applicative
import Control.Exception
import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import qualified Data.Text as T
import Data.FileEmbed
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Database.Persist.Sqlite
import Database.Esqueleto
import Control.Monad.Trans.Reader
import Data.Int

-- the DB init should be done in a try block
-- (if i don't have the rights and so on)
--
-- create a unsafePerformIO error call that displays
-- a message box, use it instead of error everywhere
-- in case of terminal failure
--
-- create db file in ~/.projectpad

import Model

migrations :: Map Int Text
migrations = Map.fromList $
	map (\(a,b) -> (parseMigrationName a, TE.decodeUtf8 b)) $(embedDir "migrations")

parseMigrationName :: FilePath -> Int
parseMigrationName name = fromMaybe (error $ "Invalid migration filename: " ++ name) convertMaybe
	where convertMaybe = if ".sql" `isSuffixOf` name && length name == 7
		then readMay (take 3 name) else Nothing
		

main :: IO ()
main = runSqlite "projectpad.db" $ do
	dbVersion <- fromMaybe 0 <$> getDbVersion
	-- if migrations fail, offer to delete the file
	upgradeFrom dbVersion
-- 	getProjects conn >>= print
	liftIO $ putStrLn "hello world"

getDbVersion :: SqlPersistM (Maybe Int)
getDbVersion = do
	hasTable <- (>0) <$> queryCount
		"select count(*) from sqlite_master where type='table' and name='db_version'"
	if hasTable
		then readDbVersion
		else return Nothing

queryCount :: Text -> SqlPersistM Int64
queryCount query = (unSingle . head) <$> rawSql query []

readDbVersion :: SqlPersistM (Maybe Int)
readDbVersion = do
	dbVersions <- select $ from $ \v -> do
		orderBy [desc (v ^. DbVersionCode)]
		limit 1
		return v
	return $ (dbVersionCode . entityVal) <$> listToMaybe dbVersions

upgradeFrom :: Int -> SqlPersistM ()
upgradeFrom curVersion = do
	let maxVersion = fromMaybe (error "No migrations") (lastZ $ Map.keys migrations)
	mapM_ applyUpgrade [(curVersion+1)..maxVersion]
-- 
applyUpgrade :: Int -> SqlPersistM ()
applyUpgrade version = do
	liftIO $ putStrLn ("applying migration " ++ show version)
	let migrationText = fromMaybe (error $ "Can't find migration " ++ show version)
		(Map.lookup version migrations)
	let commands = filter (not . T.null) (T.strip <$> T.splitOn ";" migrationText)
	mapM_ (flip rawExecute []) commands
