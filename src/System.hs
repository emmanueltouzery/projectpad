{-# LANGUAGE OverloadedStrings #-}

module System where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import System.Directory
import Control.Exception
import System.Process
import GHC.IO.Handle
import System.Environment

textEx :: SomeException -> Text
textEx = T.pack . show

runRdp :: Text -> Text -> Text -> Int -> Int -> IO (Either Text Text)
runRdp serverIp serverUsername serverPassword width height = do
	homeDir <- T.pack <$> getHomeDirectory
	let params = T.unpack <$> [serverIp,
		"-u", serverUsername,
		"-g", T.concat $ T.pack <$> [show width, "x", show height],
		"-r", T.concat ["disk:mydisk=", homeDir],
		"-p", "-"]
	r <- try (createProcess (proc "rdesktop" params) { std_in = CreatePipe})
	case r of
		Right (Just stdin, _, _, _) -> do
			hPutStr stdin (T.unpack serverPassword)
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
