{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies,
    DeriveDataTypeable, StandaloneDeriving, FlexibleContexts,
    UndecidableInstances, FlexibleInstances, ViewPatterns, ConstraintKinds #-}
module Model where

import Data.Time.Clock
import Data.ByteString
import Database.Persist.TH
import Data.Typeable
import Graphics.QML
import Database.Persist.Sql
import Database.Esqueleto (SqlEntity)
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Ord
import Data.List as L

import ModelBase
import Util

data EntityType = AllEntityTypes
                | DatabaseEntityType
                | ProjectEntityType
                | ProjectPoiEntityType
                | ProjectNoteEntityType
                | ServerEntityType
                | ServerLinkEntityType
                | ServerWebsiteEntityType
                | ServerExtraUserEntityType
                | ServerPoiEntityType
                deriving (Eq, Show, Read, Typeable)

mkPersist sqlSettings [persistLowerCase|
ServerPointOfInterest
    desc Text
    path Text
    text Text
    interestType InterestType
    groupName Text Maybe
    serverId ServerId
    deriving Show Typeable
ProjectPointOfInterest
    desc Text
    path Text
    text Text
    interestType InterestType
    groupName Text Maybe
    projectId ProjectId
    deriving Show Typeable
ProjectNote
    title Text
    contents Text
    groupName Text Maybe
    projectId ProjectId
    deriving Show Typeable
Server
    desc Text
    ip IpAddress
    text Text
    isRetired Bool
    username Text
    password Password
    authKey ByteString Maybe
    authKeyFilename Text Maybe
    type ServerType
    accessType ServerAccessType
    sshTunnelPort Int Maybe
    sshTunnelThroughServerId ServerId Maybe
    environment EnvironmentType
    groupName Text Maybe
    projectId ProjectId
    deriving Show Typeable
ServerLink
    desc Text
    linkedServerId ServerId
    environment EnvironmentType
    groupName Text Maybe
    projectId ProjectId
    deriving Show Typeable
ServerWebsite
    desc Text
    url Text
    text Text
    username Text
    password Password
    serverDatabaseId ServerDatabaseId Maybe
    groupName Text Maybe
    serverId ServerId
    deriving Show Typeable
ServerDatabase
    desc Text
    name Text
    text Text
    username Text
    password Password
    groupName Text Maybe
    serverId ServerId
    deriving Show Typeable
ServerExtraUserAccount
    username Text
    password Password
    desc Text
    authKey ByteString Maybe
    authKeyFilename Text Maybe
    groupName Text Maybe
    serverId ServerId
    deriving Show Typeable
Project
    name Text
    icon ByteString
    hasDev Bool
    hasUat Bool
    hasStage Bool
    hasProd Bool
    deriving Show Typeable
DbVersion
    code Int
    upgradeDate UTCTime
    deriving Show
|]

type EntityRef a = ObjRef (Entity a)

fromEntityRef :: EntityRef a -> a
fromEntityRef = entityVal . fromObjRef

int64to32 :: Int64 -> Int
int64to32 = fromIntegral

fromSqlKey32 :: (ToBackendKey SqlBackend a) => Entity a -> Int
fromSqlKey32 = int64to32 . fromSqlKey . entityKey

toSqlKey32 :: ToBackendKey SqlBackend record => Int -> Key record
toSqlKey32 = toSqlKey . fromIntegral

getStandardClassMembers :: (ToBackendKey SqlBackend record, Typeable record) =>
     [Member (GetObjType (EntityRef record))]
    -> [Member (GetObjType (EntityRef record))]
getStandardClassMembers others = idProperty:others
  where idProperty = defPropertyConst "id" (return . fromSqlKey32 . fromObjRef)

defPropConst :: (QmlReturnable tr, Typeable b) =>
    String -> (b -> tr) -> Member (GetObjType (EntityRef b))
defPropConst name f = defPropertyConst name (return . f . fromEntityRef)

defFk :: (ToBackendKey SqlBackend record1, Typeable record) =>
          String -> (record -> Maybe (Key record1)) -> Member (GetObjType (EntityRef record))
defFk name f = defPropertyConst name (return . getKeyM f)

getKeyM :: (ToBackendKey SqlBackend record1) =>
    (record -> Maybe (Key record1)) -> EntityRef record -> Maybe Int
getKeyM f (fromObjRef -> entity) = do
    fk <- f (entityVal entity)
    return $ int64to32 $ fromSqlKey fk

-- TODO generate this with TH?
instance DefaultClass (Entity Project) where
    classMembers = getStandardClassMembers
        [
            defPropConst "name"          projectName,
            defPropConst "hasCustomIcon" $ not . BS.null . projectIcon,
            defPropConst "hasDev"        projectHasDev,
            defPropConst "hasUat"        projectHasUat,
            defPropConst "hasStaging"    projectHasStage,
            defPropConst "hasProd"       projectHasProd
        ]

readT :: Read a => T.Text -> a
readT = read . T.unpack

instance DefaultClass (Entity Server) where
    classMembers = getStandardClassMembers
        [
            defPropConst "desc"            serverDesc,
            defPropConst "serverIp"        serverIp,
            defPropConst "isRetired"       serverIsRetired,
            defPropConst "text"            serverText,
            defPropConst "username"        serverUsername,
            defPropConst "password"        serverPassword,
            defPropConst "authKeyFilename" $ fromMaybe "..." . serverAuthKeyFilename,
            defPropConst "type"            $ text . serverType,
            defPropConst "accessType"      $ text . serverAccessType,
            defPropConst "sshTunnelPort"   serverSshTunnelPort,
            defFk        "sshTunnelThroughServerId" serverSshTunnelThroughServerId,
            defPropConst "environment"     $ text . serverEnvironment,
            defPropConst "groupName"       $ fromMaybe "" . serverGroupName,
            defFk        "projectId"       $ Just . serverProjectId
        ]

instance DefaultClass (Entity ServerLink) where
    classMembers = getStandardClassMembers
        [
            defPropConst "desc"            serverLinkDesc,
            defFk        "linkedServerId"  $ Just . serverLinkLinkedServerId,
            defPropConst "environment"     $ text . serverLinkEnvironment,
            defPropConst "groupName"       $ fromMaybe "" . serverLinkGroupName,
            defFk        "projectId"       $ Just . serverLinkProjectId
        ]

instance DefaultClass (Entity ServerPointOfInterest) where
    classMembers = getStandardClassMembers
        [
            defPropConst "desc"         serverPointOfInterestDesc,
            defPropConst "path"         serverPointOfInterestPath,
            defPropConst "text"         serverPointOfInterestText,
            defPropConst "interestType" $ text . serverPointOfInterestInterestType,
            defPropConst "groupName"    $ fromMaybe "" . serverPointOfInterestGroupName
        ]

instance DefaultClass (Entity ServerWebsite) where
    classMembers = getStandardClassMembers
        [
            defPropConst "desc"      serverWebsiteDesc,
            defPropConst "url"       serverWebsiteUrl,
            defPropConst "text"      serverWebsiteText,
            defPropConst "username"  serverWebsiteUsername,
            defPropConst "password"  serverWebsitePassword,
            defPropConst "groupName" $ fromMaybe "" . serverWebsiteGroupName,
            defFk "serverDatabaseId" serverWebsiteServerDatabaseId
        ]

instance DefaultClass (Entity ServerDatabase) where
    classMembers = getStandardClassMembers
        [
            defPropConst "desc"      serverDatabaseDesc,
            defPropConst "name"      serverDatabaseName,
            defPropConst "text"      serverDatabaseText,
            defPropConst "username"  serverDatabaseUsername,
            defPropConst "password"  serverDatabasePassword,
            defPropConst "groupName" $ fromMaybe "" . serverDatabaseGroupName,
            defFk "serverId" $ Just . serverDatabaseServerId
        ]

instance DefaultClass (Entity ServerExtraUserAccount) where
    classMembers = getStandardClassMembers
        [
            defPropConst "username"        serverExtraUserAccountUsername,
            defPropConst "password"        serverExtraUserAccountPassword,
            defPropConst "desc"            serverExtraUserAccountDesc,
            defPropConst "authKeyFilename" $ fromMaybe "..." . serverExtraUserAccountAuthKeyFilename,
            defPropConst "groupName"       $ fromMaybe "" . serverExtraUserAccountGroupName
        ]

instance DefaultClass (Entity ProjectPointOfInterest) where
    classMembers = getStandardClassMembers
        [
            defPropConst "desc"         projectPointOfInterestDesc,
            defPropConst "path"         projectPointOfInterestPath,
            defPropConst "text"         projectPointOfInterestText,
            defPropConst "interestType" $ text . projectPointOfInterestInterestType,
            defPropConst "groupName"    $ fromMaybe "" . projectPointOfInterestGroupName
        ]

instance DefaultClass (Entity ProjectNote) where
    classMembers = getStandardClassMembers
        [
            defPropConst "title"     projectNoteTitle,
            defPropConst "contents"  projectNoteContents,
            defPropConst "groupName" $ fromMaybe "" . projectNoteGroupName
        ]

deriving instance Typeable Entity
deriving instance Typeable Key

runSqlBackend :: SqlBackend -> SqlPersistM a -> IO a
runSqlBackend = flip runSqlPersistM

sqlToQml :: DefaultClass a => SqlBackend -> SqlPersistM [a] -> IO [ObjRef a]
sqlToQml sqlBackend f = mapM newObjectDC =<< runSqlBackend sqlBackend f

-- I have decided in the schema to have null for "no group".
-- In that sense, "" makes no sense, because it would also
-- mean "no group"... So make sure we never send "" to the DB.
-- I also have a constraint there, length>0.
groupOrNothing :: Maybe Text -> Maybe Text
groupOrNothing (Just "") = Nothing
groupOrNothing x@_ = x

readEntityFields :: SqlBackend -> SqlPersistM [Entity a] -> (a -> b) -> IO [b]
readEntityFields sqlBackend r f =
    fmap (f . entityVal) <$> runSqlBackend sqlBackend r

readEntityField :: (ToBackendKey SqlBackend a, SqlEntity a) =>
                    SqlBackend -> Int -> (a -> b) -> IO (Maybe b)
readEntityField sqlBackend entKey f =
    fmap f <$> runSqlBackend sqlBackend (get $ toSqlKey32 entKey)

mergeNames :: [Maybe Text] -> [Text]
mergeNames = nub . sortBy (comparing T.toCaseFold) . catMaybes

filterForGroup :: Eq b => b -> (a -> b) -> [EntityRef a] -> [EntityRef a]
filterForGroup grp grpNameField = L.filter ((==grp) . grpNameField . fromEntityRef)
