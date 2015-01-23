{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ViewPatterns #-}
module ProjectView where

import Control.Applicative
import Control.Concurrent.MVar
import Graphics.QML
import Database.Esqueleto
import qualified Database.Persist as P
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Process
import Control.Exception
import System.Environment
import GHC.IO.Handle
import qualified Data.Map as M
import Data.Maybe
import Control.Arrow
import Control.Error
import System.Directory

import ModelBase
import Model
import ChildEntityCache

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

runRdp :: ObjRef (Entity Server) -> Int -> Int -> IO (Either Text Text)
runRdp (entityVal . fromObjRef -> server) width height = do
	homeDir <- T.pack <$> getHomeDirectory
	let params = T.unpack <$> [serverIp server,
		"-u", serverUsername server,
		"-g", T.concat $ T.pack <$> [show width, "x", show height],
		"-r", T.concat ["disk:mydisk=", homeDir],
		"-p", "-"]
	r <- try (createProcess (proc "rdesktop" params) { std_in = CreatePipe})
	case r of
		Right (Just stdin, _, _, _) -> do
			hPutStr stdin (T.unpack $ serverPassword server)
			hFlush stdin
			return $ Right ""
		Left x -> return $ Left $ textEx x
		_ -> error "run RDP unexpected process output"

tryCommand :: String -> [String] -> IO (Either Text Text)
tryCommand cmd params = do
	r <- try (createProcess (proc cmd params) {std_out = CreatePipe})
	case r of
		Right (_, Just stdout, _, _) -> Right <$> T.pack <$> hGetContents stdout
		Left (SomeException x) -> return $ Left $ T.pack $ show x
		_ -> error "Try command unexpected process output"

openAssociatedFile :: Text -> IO (Either Text Text)
openAssociatedFile path = do
	desktop <- getEnv "DESKTOP_SESSION"
	let openCmd = case desktop of
		"gnome" -> "gnome-open"
		_ -> "xdg-open"
	tryCommand openCmd [T.unpack path]

runPoiAction :: ObjRef ProjectViewState
	-> ObjRef (Entity ProjectPointOfInterest) -> IO (Either Text Text)
runPoiAction _ (entityVal . fromObjRef -> poi)
	| interest == PoiCommandToRun = do
		let (prog:parameters) = T.unpack <$> T.splitOn " " path
		tryCommand prog parameters
	| interest == PoiLogFile = openAssociatedFile path
	| otherwise = return $ Left "not handled"
	where
		interest = projectPointOfInterestInterestType poi 
		path = projectPointOfInterestPath poi

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

textEx :: SomeException -> Text
textEx = text

serializeEither :: Either Text Text -> [Text]
serializeEither (Left x) = ["error", x]
serializeEither (Right x) = ["success", x]

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
	return $ M.fromList $ fmap (unValue *** unValue) $ poiInfosVal

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
			defMethod "runPoiAction" (\state projectPoi -> serializeEither <$>
				runPoiAction state projectPoi),
			defMethod "saveAuthKey" (\state path server -> serializeEither <$>
				saveAuthKey state path server),
			defMethod' "runRdp" (\_ server width height -> serializeEither <$>
				runRdp server width height)
		]
	newObject projectViewClass projectViewState
