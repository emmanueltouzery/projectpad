{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies,
             FlexibleInstances, ConstraintKinds #-}
module Util where

import Control.Applicative
import Data.Text (Text)
import Control.Error
import Graphics.QML
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Exception
import System.FilePath.Posix
import System.Directory
import Data.Attoparsec.Text hiding (try)
import Data.Monoid

projectPadFolder :: String
projectPadFolder = ".projectpad"

getAppDir :: IO String
getAppDir = (</> projectPadFolder) <$> getHomeDirectory

-- TODO a more general version is provided by Data.Bifunctor
-- in base 4.8 (GHC 7.10). Switch when upgrading.
bimap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
bimap f _ (Left a)  = Left (f a)
bimap _ g (Right b) = Right (g b)

text :: Show a => a -> Text
text = T.pack . show

data CommandProgress = CommandOutput Text
    | CommandSucceeded
    | CommandFailed Text
    deriving Show

eitherToCmdProgress :: Either Text b -> CommandProgress
eitherToCmdProgress (Right _) = CommandSucceeded
eitherToCmdProgress (Left x)  = CommandFailed x

cmdProgressToJs :: CommandProgress -> [Text]
cmdProgressToJs (CommandOutput x) = ["text", x]
cmdProgressToJs CommandSucceeded  = ["succeeded", ""]
cmdProgressToJs (CommandFailed x) = ["failed", x]

splitParams :: Text -> Either String [Text]
splitParams = parseOnly splitParamsParser
    where
        splitParamsParser = (parseQuotedParam <|> parseParam) `sepBy` char ' ' <* endOfInput
        parseQuotedParam = char '"' *> takeWhile1 (/= '"') <* char '"'
        parseParam = takeWhile1 (/= ' ')

withParams :: Text -> (CommandProgress -> a) -> ([Text] -> a) -> a
withParams txt notify action = case splitParams txt of
  Left x   -> notify (CommandFailed $ "Error parsing the command: " <> T.pack x)
  Right [] -> notify (CommandFailed "Incomplete command line")
  Right params -> action params

data SignalOutput deriving Typeable
instance SignalKeyClass SignalOutput where
    type SignalParams SignalOutput = [Text] -> IO ()

processAuthKeyInfo :: Text -> IO (Maybe (BS.ByteString, Text))
processAuthKeyInfo keyPath = traverse getInfo $ T.stripPrefix "file://" keyPath
    where getInfo p = do
              contents <- BS.readFile (T.unpack p)
              return (contents, last $ T.splitOn "/" p)

tryText :: IO a -> IO (Either Text a)
tryText = fmap (fmapL textEx) . try

textEx :: SomeException -> Text
textEx = T.pack . show

defStatic :: (MethodSuffix ms, Typeable obj) => String -> ms -> Member obj
defStatic str cb = defMethod' str (const cb)

readM :: (a -> b) -> ObjRef a -> IO b
readM f = return . f . fromObjRef

newtype QmlResult a = QmlResult (Either Text a) deriving (Show, Typeable)

unQmlResult :: QmlResult a -> Either Text a
unQmlResult (QmlResult x) = x

type QmlReturnable a = (Marshal a, MarshalMode a ICanReturnTo () ~ Yes)

defQmlResultProp :: (Typeable a, QmlReturnable b) =>
    String -> (Either Text a -> b) -> Member (GetObjType (ObjRef (QmlResult a)))
defQmlResultProp name f = defPropertyConst name $ return . f . unQmlResult . fromObjRef

instance (Typeable a, QmlReturnable a) => DefaultClass (QmlResult a) where
    classMembers =
        [
            defQmlResultProp "success"  isRight,
            defQmlResultProp "value"    hush,
            defQmlResultProp "errorMsg" (\(Left x) -> x)
        ]

liftQmlResult :: (Typeable a, QmlReturnable a) =>
                 IO (Either Text a) -> IO (ObjRef (QmlResult a))
liftQmlResult r = newObjectDC =<< (QmlResult <$> r)

liftQmlResult1 :: (Typeable a, QmlReturnable a) =>
                  (x -> IO (Either Text a)) -> (x -> IO (ObjRef (QmlResult a)))
liftQmlResult1 f p = liftQmlResult $ f p

liftQmlResult2 :: (Typeable a, QmlReturnable a) =>
                  (x -> y -> IO (Either Text a)) -> (x -> y -> IO (ObjRef (QmlResult a)))
liftQmlResult2 f p q = liftQmlResult $ f p q

liftQmlResult3 :: (Typeable a, QmlReturnable a) =>
                  (x -> y -> z -> IO (Either Text a)) -> (x -> y -> z -> IO (ObjRef (QmlResult a)))
liftQmlResult3 f p q r = liftQmlResult $ f p q r

noteET :: Monad m => a -> Maybe b -> ExceptT a m b
noteET l = hoistEither . note l

tryET :: IO a -> ExceptT Text IO a
tryET = ExceptT . tryText

readTZ :: Read a => Text -> Maybe a
readTZ = readZ . T.unpack
