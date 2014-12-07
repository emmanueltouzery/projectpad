{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Database.SQLite.Simple
import qualified Database.SQLite3 as Direct
import Control.Error
import Control.Applicative
import Control.Exception
import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Data.FileEmbed
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

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
main = do
	conn <- open "projectpad.db"
	dbVersion <- fromMaybe 0 <$> getDbVersion conn
	-- if migrations fail, offer to delete the file
	upgradeFrom conn dbVersion
	putStrLn "hello world"
	close conn

getDbVersion :: Connection -> IO (Maybe Int)
getDbVersion conn = runMaybeT $ do
	-- if the table doesn't exist, I'll get an SQLError.
	let queryStr = "select * from db_version order by version_code desc limit 1"
	rows <- hushT (EitherT $ (trySql (query_ conn queryStr)))
	hoistMaybe $ versionCode <$> headMay rows
	where
		trySql :: IO a -> IO (Either Direct.SQLError a)
		trySql = try

upgradeFrom :: Connection -> Int -> IO ()
upgradeFrom conn curVersion = do
	let maxVersion = fromMaybe (error "No migrations") (lastZ $ Map.keys migrations)
	mapM_ (applyUpgrade conn) [(curVersion+1)..maxVersion]

applyUpgrade :: Connection -> Int -> IO ()
applyUpgrade conn version = do
	putStrLn ("applying migration " ++ show version)
	let migrationText = fromMaybe (error $ "Can't find migration " ++ show version)
		(Map.lookup version migrations)
	print migrationText
	Direct.exec (connectionHandle conn) migrationText
