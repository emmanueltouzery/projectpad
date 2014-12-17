{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
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

import Model
import Schema

main :: IO ()
main = runSqlite "projectpad.db" $ do
	upgradeSchema
	--P.insert (Project "LTA" "")
	projects <- select $ from $ \p -> do
		orderBy [asc (p ^. ProjectName)]
		return p
	liftIO $ print $ entityVal <$> projects
