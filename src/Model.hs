{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, ViewPatterns #-}
module Model where

import Data.Time.Clock
import Data.ByteString
import Database.Persist.TH
import Data.Typeable
import Graphics.QML
import Database.Persist.Sql
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Maybe
import Control.Applicative
import Data.Ord
import Data.List as L

import ModelBase

-- TODO Project hasDev, Uat and so on should be Bool...
-- but currently fails with PersistMarshalError (persistent 2.1.1.4)
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
    username Text
    password Password
    authKey ByteString Maybe
    authKeyFilename Text Maybe
    type ServerType
    accessType ServerAccessType
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
    hasDev Text
    hasUat Text
    hasStage Text
    hasProd Text
    deriving Show Typeable
DbVersion
    code Int
    upgradeDate UTCTime
    deriving Show
|]

int64to32 :: Int64 -> Int
int64to32 = fromIntegral

fromSqlKey32 :: (ToBackendKey SqlBackend a) => Entity a -> Int
fromSqlKey32 = int64to32 . fromSqlKey . entityKey

toSqlKey32 :: ToBackendKey SqlBackend record => Int -> Key record
toSqlKey32 = toSqlKey . fromIntegral

getStandardClassMembers :: (ToBackendKey SqlBackend record, Typeable record) =>
     [Member (GetObjType (ObjRef (Entity record)))]
    -> [Member (GetObjType (ObjRef (Entity record)))]
getStandardClassMembers others = idProperty:others
  where idProperty = defPropertyConst "id" (return . fromSqlKey32 . fromObjRef)

defPropConst :: (Marshal tr, Typeable b, MarshalMode tr ICanReturnTo () ~ Yes) =>
    String -> (b -> tr) -> Member (GetObjType (ObjRef (Entity b)))
defPropConst name f = defPropertyConst name (return . f . entityVal . fromObjRef)

defFk :: (ToBackendKey SqlBackend record1, Typeable record) =>
          String -> (record -> Maybe (Key record1)) -> Member (GetObjType (ObjRef (Entity record)))
defFk name f = defPropertyConst name (return . getKeyM f)

getKeyM :: (ToBackendKey SqlBackend record1) =>
    (record -> Maybe (Key record1)) -> ObjRef (Entity record) -> Maybe Int
getKeyM f (fromObjRef -> entity) = do
    fk <- f (entityVal entity)
    return $ int64to32 $ fromSqlKey fk

-- TODO generate this with TH?
instance DefaultClass (Entity Project) where
    classMembers = getStandardClassMembers
        [
            defPropConst "name"          projectName,
            defPropConst "hasCustomIcon" $ not . BS.null . projectIcon,
            defPropConst "hasDev"        $ readBool . projectHasDev,
            defPropConst "hasUat"        $ readBool . projectHasUat,
            defPropConst "hasStaging"    $ readBool . projectHasStage,
            defPropConst "hasProd"       $ readBool . projectHasProd
        ]

readBool :: Text -> Bool
readBool = read . T.unpack

text :: Show a => a -> Text
text = T.pack . show

readT :: Read a => T.Text -> a
readT = read . T.unpack

instance DefaultClass (Entity Server) where
    classMembers = getStandardClassMembers
        [
            defPropConst "desc" serverDesc,
            defPropConst "serverIp" serverIp,
            defPropConst "text" serverText,
            defPropConst "username" serverUsername,
            defPropConst "password" serverPassword,
            defPropConst "authKeyFilename" $ fromMaybe "..." . serverAuthKeyFilename,
            defPropConst "type" $ text . serverType,
            defPropConst "accessType" $ text . serverAccessType,
            defPropConst "environment" $ text . serverEnvironment,
            defPropConst "groupName" $ fromMaybe "" . serverGroupName,
            defFk "projectId" $ Just . serverProjectId
        ]

instance DefaultClass (Entity ServerPointOfInterest) where
    classMembers = getStandardClassMembers
        [
            defPropConst "desc" serverPointOfInterestDesc,
            defPropConst "path" serverPointOfInterestPath,
            defPropConst "text" serverPointOfInterestText,
            defPropConst "interestType" $ text . serverPointOfInterestInterestType,
            defPropConst "groupName" $ fromMaybe "" . serverPointOfInterestGroupName
        ]

instance DefaultClass (Entity ServerWebsite) where
    classMembers = getStandardClassMembers
        [
            defPropConst "desc" serverWebsiteDesc,
            defPropConst "url" serverWebsiteUrl,
            defPropConst "text" serverWebsiteText,
            defPropConst "username" serverWebsiteUsername,
            defPropConst "password" serverWebsitePassword,
            defPropConst "groupName" $ fromMaybe "" . serverWebsiteGroupName,
            defFk "serverDatabaseId" serverWebsiteServerDatabaseId
        ]

instance DefaultClass (Entity ServerDatabase) where
    classMembers = getStandardClassMembers
        [
            defPropConst "desc" serverDatabaseDesc,
            defPropConst "name" serverDatabaseName,
            defPropConst "text" serverDatabaseText,
            defPropConst "username" serverDatabaseUsername,
            defPropConst "password" serverDatabasePassword,
            defPropConst "groupName" $ fromMaybe "" . serverDatabaseGroupName
        ]

instance DefaultClass (Entity ServerExtraUserAccount) where
    classMembers = getStandardClassMembers
        [
            defPropConst "username" serverExtraUserAccountUsername,
            defPropConst "password" serverExtraUserAccountPassword,
            defPropConst "desc" serverExtraUserAccountDesc,
            defPropConst "authKeyFilename" $ fromMaybe "..." . serverExtraUserAccountAuthKeyFilename,
            defPropConst "groupName" $ fromMaybe "" . serverExtraUserAccountGroupName
        ]

instance DefaultClass (Entity ProjectPointOfInterest) where
    classMembers = getStandardClassMembers
        [
            defPropConst "desc" projectPointOfInterestDesc,
            defPropConst "path" projectPointOfInterestPath,
            defPropConst "text" projectPointOfInterestText,
            defPropConst "interestType" $ text . projectPointOfInterestInterestType,
            defPropConst "groupName" $ fromMaybe "" . projectPointOfInterestGroupName
        ]

instance DefaultClass (Entity ProjectNote) where
    classMembers = getStandardClassMembers
        [
            defPropConst "title" projectNoteTitle,
            defPropConst "contents" projectNoteContents,
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

readEntityField :: SqlBackend -> SqlPersistM [Entity a] -> (a -> b) -> IO [b]
readEntityField sqlBackend r f =
    fmap (f . entityVal) <$> runSqlBackend sqlBackend r

mergeNames :: [Maybe Text] -> [Text]
mergeNames = nub . sortBy (comparing T.toCaseFold) . catMaybes

filterForGroup :: Eq b => b -> (a -> b) -> [ObjRef (Entity a)] -> [ObjRef (Entity a)]
filterForGroup grp grpNameField = L.filter ((==grp) . grpNameField . entityVal . fromObjRef)
