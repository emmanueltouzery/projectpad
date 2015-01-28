{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ViewPatterns #-}
module ProjectView where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import Graphics.QML.Objects.ParamNames
import Database.Esqueleto
import qualified Database.Persist as P
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Control.Exception
import qualified Data.Map as M
import Data.Maybe
import Control.Arrow
import Control.Error

import ModelBase
import Model
import ChildEntityCache
import System
import Util

data ProjectViewState = ProjectViewState
	{
		curProjectId :: MVar (Maybe Int),
		servers :: EntityListCache Server,
		pois :: EntityListCache ProjectPointOfInterest
	} deriving Typeable

instance DynParentHolder ProjectViewState where
	dynParentId = curProjectId
	clearAllChildrenCaches state = do
		swapMVar_ (servers state) Nothing
		swapMVar_ (pois state) Nothing

instance CacheHolder Server ProjectViewState where
	cacheChildren = servers

instance CacheHolder ProjectPointOfInterest ProjectViewState where
	cacheChildren = pois

readServers :: Int -> SqlPersistM [Entity Server]
readServers projectId = select $ from $ \s -> do
	where_ (s ^. ServerProjectId ==. val (toSqlKey32 projectId))
	orderBy [asc (s ^. ServerDesc)]
	return s

processAuthKeyInfo :: Text -> IO (Maybe (BS.ByteString, Text))
processAuthKeyInfo keyPath = case T.stripPrefix "file://" keyPath of
	Nothing -> return Nothing
	Just p -> do
		contents <- BS.readFile (T.unpack p)
		return $ Just (contents, last $ T.splitOn "/" p)

addServer :: SqlBackend -> ObjRef ProjectViewState
	-> Text -> IpAddress -> Text -> Text -> Text -> Text -> Text -> IO ()
addServer sqlBackend stateRef sDesc ipAddr username password
		keyPath serverTypeT serverAccessTypeT = do
	let srvType = read $ T.unpack serverTypeT
	let srvAccessType = read $ T.unpack serverAccessTypeT
	authKeyInfo <- processAuthKeyInfo keyPath
	addHelper sqlBackend stateRef readServers
		$ Server sDesc ipAddr username password
			(fst <$> authKeyInfo) (snd <$> authKeyInfo) srvType srvAccessType

updateServer :: SqlBackend -> ObjRef ProjectViewState -> ObjRef (Entity Server)
	-> Text -> IpAddress -> Text -> Text -> Text -> Text -> Text -> IO (ObjRef (Entity Server))
updateServer sqlBackend stateRef serverRef sDesc ipAddr username password
		keyPath serverTypeT serverAccessTypeT = do
	let srvType = read $ T.unpack serverTypeT
	let srvAccessType = read $ T.unpack serverAccessTypeT
	authKeyInfo <- processAuthKeyInfo keyPath
	updateHelper sqlBackend stateRef serverRef readServers servers
		[
			ServerDesc P.=. sDesc, ServerIp P.=. ipAddr,
			ServerUsername P.=. username, ServerPassword P.=. password,
			ServerType P.=. srvType, ServerAccessType P.=. srvAccessType,
			ServerAuthKey P.=. fst <$> authKeyInfo,
			ServerAuthKeyFilename P.=. snd <$> authKeyInfo
		]

deleteServers :: SqlBackend -> ObjRef ProjectViewState -> [Int] -> IO ()
deleteServers = deleteHelper convertKey readServers

readPois :: Int -> SqlPersistM [Entity ProjectPointOfInterest]
readPois projectId = select $ from $ \poi -> do
	where_ (poi ^. ProjectPointOfInterestProjectId ==. val (toSqlKey32 projectId))
	orderBy [asc (poi ^. ProjectPointOfInterestDesc)]
	return poi

addProjectPoi :: SqlBackend -> ObjRef ProjectViewState
	-> Text -> Text -> Text -> Text -> IO ()
addProjectPoi sqlBackend stateRef
	pDesc path txt interestTypeT = do
	let interestType = read $ T.unpack interestTypeT
	addHelper sqlBackend stateRef readPois
		$ ProjectPointOfInterest pDesc path txt interestType

updateProjectPoi :: SqlBackend -> ObjRef ProjectViewState -> ObjRef (Entity ProjectPointOfInterest)
	-> Text -> Text -> Text -> Text -> IO (ObjRef (Entity ProjectPointOfInterest))
updateProjectPoi sqlBackend stateRef poiRef
	pDesc path txt interestTypeT = do
	let interestType = read $ T.unpack interestTypeT
	updateHelper sqlBackend stateRef poiRef readPois pois
		[
			ProjectPointOfInterestDesc P.=. pDesc, ProjectPointOfInterestPath P.=. path,
			ProjectPointOfInterestText P.=. txt,
			ProjectPointOfInterestInterestType P.=. interestType
		]

deleteProjectPois :: SqlBackend -> ObjRef ProjectViewState -> [Int] -> IO ()
deleteProjectPois = deleteHelper convertKey readPois

runPoiAction :: ObjRef ProjectViewState
	-> ObjRef (Entity ProjectPointOfInterest) -> IO ()
runPoiAction prjViewState (entityVal . fromObjRef -> poi)
	| interest == PoiCommandToRun = do
		let (prog:parameters) = T.unpack <$> T.splitOn " " txt
		tryCommandAsync prog parameters path Nothing
			(fireSignal (Proxy :: Proxy SignalOutput) prjViewState . cmdProgressToJs)
	| interest == PoiLogFile = do
		result <- openAssociatedFile (projectPointOfInterestPath poi)
		fireSignal (Proxy :: Proxy SignalOutput) prjViewState (cmdProgressToJs $ eitherToCmdProgress result)
	| otherwise = putStrLn "poi action not handled"
	where
		interest = projectPointOfInterestInterestType poi 
		path = case T.unpack $ projectPointOfInterestPath poi of
			"" -> Nothing
			x@_ -> Just x
		txt = projectPointOfInterestText poi

-- alternative implementations: http://stackoverflow.com/a/28101291/516188
saveAuthKey :: ObjRef ProjectViewState
	-> Text -> ObjRef (Entity Server) -> IO (Either Text Text)
saveAuthKey _ path (entityVal . fromObjRef -> server) = runEitherT $ do
	targetFile <- hoistEither $ note "Invalid target file name"
		$ T.stripPrefix "file://" path
	key <- hoistEither $ note "No authentication key for that server!"
		$ serverAuthKey server
	bimapEitherT textEx (const "") . EitherT . try
		$ BS.writeFile (T.unpack targetFile) key

runServerRdp :: ObjRef (Entity Server) -> Int -> Int -> IO (Either Text Text)
runServerRdp (entityVal . fromObjRef -> server) =
	runRdp (serverIp server) (serverUsername server) (serverPassword server)

openServerSshSession :: ObjRef (Entity Server) -> IO (Either Text Text)
openServerSshSession (entityVal . fromObjRef -> server) = fmapR (const "") <$>
	openSshSession (serverIp server) (serverUsername server) (serverPassword server)

data ServerExtraInfo = ServerExtraInfo
	{
		srvExtraInfoServer :: ObjRef (Entity Server),
		srvExtraInfoPoiCount :: Int,
		srvExtraInfoWwwCount :: Int,
		srvExtraInfoDbCount :: Int
	} deriving Typeable

-- server entity, together with some stats about the
-- entities inside the server.
instance DefaultClass ServerExtraInfo where
	classMembers =
		[
			defPropertyConst "server" (return . srvExtraInfoServer . fromObjRef),
			defPropertyConst "poiCount" (return . srvExtraInfoPoiCount . fromObjRef),
			defPropertyConst "wwwCount" (return . srvExtraInfoWwwCount . fromObjRef),
			defPropertyConst "dbCount" (return . srvExtraInfoDbCount . fromObjRef)
		]

readServersExtraInfo :: (PersistEntity val,
	PersistField typ1, PersistField typ, PersistEntityBackend val ~ SqlBackend) =>
	EntityField val typ1 -> EntityField val typ -> [typ] -> SqlPersistM [(Value typ, Value Int)]
readServersExtraInfo tableId serverFk serverIds = select $ from $ \sp -> do
		groupBy (sp ^. serverFk)
		having (sp ^. serverFk `in_` valList serverIds)
		return (sp ^. serverFk, count (sp ^. tableId))

getInfosVal :: (PersistEntity val, PersistField typ1, PersistEntityBackend val ~ SqlBackend)
	=> SqlBackend -> M.Map (Key Server) a -> EntityField val typ1
	-> EntityField val (Key Server) -> IO (M.Map (Key Server) Int)
getInfosVal sqlBackend serversById tableId serverFk = do
	poiInfosVal <- runSqlBackend sqlBackend $ readServersExtraInfo
		tableId serverFk $ M.keys serversById
	return $ M.fromList $ fmap (unValue *** unValue) poiInfosVal

getServersExtraInfo :: SqlBackend -> ObjRef ProjectViewState -> Int -> IO [ObjRef ServerExtraInfo]
getServersExtraInfo sqlBackend projectViewState projectId = do
	prjServers <- getChildren sqlBackend readServers projectViewState projectId
	let serversById = M.fromList $ fmap (\s -> (objRefKey s, s)) prjServers

	poiInfos <- getInfosVal sqlBackend serversById
		ServerPointOfInterestId ServerPointOfInterestServerId
	wwwInfos <- getInfosVal sqlBackend serversById
		ServerWebsiteId ServerWebsiteServerId
	dbInfos <- getInfosVal sqlBackend serversById
		ServerDatabaseId ServerDatabaseServerId

	mapM (\s -> newObjectDC $ ServerExtraInfo s
		(getServerCount s poiInfos)
		(getServerCount s wwwInfos)
		(getServerCount s dbInfos)) prjServers
	where
		objRefKey = entityKey . fromObjRef
		getServerCount s = fromMaybe 0 . M.lookup (objRefKey s)

createProjectViewState :: SqlBackend -> IO (ObjRef ProjectViewState)
createProjectViewState sqlBackend = do
	projectViewState <- ProjectViewState
		<$> newMVar Nothing
		<*> newMVar Nothing
		<*> newMVar Nothing
	projectViewClass <- newClass
		[
			defMethod "getServers" (getServersExtraInfo sqlBackend),
			defMethod "addServer" (addServer sqlBackend),
			defMethod "updateServer" (updateServer sqlBackend),
			defMethod "deleteServers" (deleteServers sqlBackend),
			defMethod "getPois" (getChildren sqlBackend readPois),
			defMethod "addProjectPoi" (addProjectPoi sqlBackend),
			defMethod "updateProjectPoi" (updateProjectPoi sqlBackend),
			defMethod "deleteProjectPois" (deleteProjectPois sqlBackend),
			defMethod' "runPoiAction" runPoiAction,
			defMethod "saveAuthKey" (\state path server -> serializeEither <$>
				saveAuthKey state path server),
			defMethod' "runRdp" (\_ server width height -> serializeEither <$>
				runServerRdp server width height),
			defMethod' "openSshSession" (\_ server -> serializeEither <$>
				openServerSshSession server),
			defSignalNamedParams "gotOutput" (Proxy :: Proxy SignalOutput) $
				fstName "output"
		]
	newObject projectViewClass projectViewState
