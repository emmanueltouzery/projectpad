{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, FlexibleInstances #-}
module Util where

import Data.Text (Text)
import Control.Error
import Graphics.QML
import Data.Typeable
import Data.Traversable
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Exception
import Control.Applicative
import Data.Attoparsec.Text

-- TODO a more general version is provided by Data.Bifunctor
-- in base 4.8 (GHC 7.10). Switch when upgrading.
bimap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
bimap f _ (Left a) = Left (f a)
bimap _ g (Right b) = Right (g b)

serializeEither :: Either Text Text -> [Text]
serializeEither (Left x) = ["error", x]
serializeEither (Right x) = ["success", x]

serializeEither' :: Either Text () -> [Text]
serializeEither' = serializeEither . fmapR (const "")

data CommandProgress = CommandOutput Text
    | CommandSucceeded
    | CommandFailed Text

eitherToCmdProgress :: Either Text b -> CommandProgress
eitherToCmdProgress (Right _) = CommandSucceeded
eitherToCmdProgress (Left x) = CommandFailed x

cmdProgressToJs :: CommandProgress -> [Text]
cmdProgressToJs (CommandOutput x) = ["text", x]
cmdProgressToJs CommandSucceeded = ["succeeded", ""]
cmdProgressToJs (CommandFailed x) = ["failed", x]

data SignalOutput deriving Typeable
instance SignalKeyClass SignalOutput where
    type SignalParams SignalOutput = [Text] -> IO ()

processAuthKeyInfo :: Text -> IO (Maybe (BS.ByteString, Text))
processAuthKeyInfo keyPath = traverse getInfo $ T.stripPrefix "file://" keyPath
    where getInfo p = do
              contents <- BS.readFile (T.unpack p)
              return (contents, last $ T.splitOn "/" p)

textEx :: SomeException -> Text
textEx = T.pack . show

defStatic :: (MethodSuffix ms, Typeable obj) => String -> ms -> Member obj
defStatic str cb = defMethod' str (const cb)

readM :: (a -> b) -> ObjRef a -> IO b
readM f = return . f . fromObjRef

newtype QmlResult a = QmlResult (Either Text a)

instance Marshal (QmlResult Text) where
    type MarshalMode (QmlResult Text) c d = ModeTo c
    marshaller = toMarshaller $ \(QmlResult x) -> serializeEither x

instance Marshal (QmlResult ()) where
    type MarshalMode (QmlResult ()) c d = ModeTo c
    marshaller = toMarshaller $ \(QmlResult x) -> serializeEither' x

liftQmlResult :: IO (Either Text a) -> IO (QmlResult a)
liftQmlResult = fmap QmlResult

liftQmlResult1 :: (x -> IO (Either Text a)) -> (x -> IO (QmlResult a))
liftQmlResult1 f p =  QmlResult <$> f p

liftQmlResult2 :: (x -> y -> IO (Either Text a)) -> (x -> y -> IO (QmlResult a))
liftQmlResult2 f p q =  QmlResult <$> f p q

liftQmlResult3 :: (x -> y -> z -> IO (Either Text a)) -> (x -> y -> z -> IO (QmlResult a))
liftQmlResult3 f p q r =  QmlResult <$> f p q r

attoParse :: Parser a -> Text -> Either String a
attoParse parser = eitherResult . flip feed T.empty . parse parser
