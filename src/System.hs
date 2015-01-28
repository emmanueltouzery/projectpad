{-# LANGUAGE OverloadedStrings #-}

module System where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative
import System.Directory
import Control.Exception
import System.Process
import GHC.IO.Handle
import System.Environment
import System.Posix.Files
import System.FilePath.Posix
import Control.Error
import Control.Monad
import Control.Concurrent

import Util

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

tryCommand :: String -> [String] -> Maybe FilePath
	-> Maybe [(String, String)] -> (Text -> IO ()) -> IO (Either Text ())
tryCommand cmd params mCwd envVal readCallback = do
	r <- try (createProcess (proc cmd params)
		{
			std_out = CreatePipe,
			env = envVal,
			cwd = mCwd
		})
	case r of
		Right (_, Just stdout, _, _) -> Right <$> readHandleProgress stdout readCallback
		Left (SomeException x) -> return $ Left $ T.pack $ show x
		_ -> error "Try command unexpected process output"

readHandleProgress :: Handle -> (Text -> IO ()) -> IO ()
readHandleProgress hndl readCallback = do
	chunk <- T.hGetChunk hndl
	readCallback chunk
	when (not $ T.null chunk) $
		readHandleProgress hndl readCallback

tryCommandAsync :: String -> [String] -> Maybe FilePath
	-> Maybe [(String, String)] -> (CommandProgress -> IO ())  -> IO ()
tryCommandAsync cmd params mCwd envVal readCallback = do
	let runCmd = tryCommand cmd params mCwd envVal
		(readCallback . CommandOutput)
	void $ forkFinally runCmd (readCallback . convert)
	where
		convert (Right x) = eitherToCmdProgress x
		convert (Left x) = CommandFailed $ textEx x

openAssociatedFile :: Text -> IO (Either Text ())
openAssociatedFile path = do
	desktop <- getEnv "DESKTOP_SESSION"
	let openCmd = case desktop of
		"gnome" -> "gnome-open"
		_ -> "xdg-open"
	tryCommand openCmd [T.unpack path] Nothing Nothing (const $ return ())

writeTempScript :: String -> Text -> IO ()
writeTempScript fname contents = do
	T.writeFile fname contents
	setFileMode fname ownerModes

runSshContents :: Text -> Text -> Text
runSshContents hostname username = T.concat ["#!/usr/bin/sh\n\
	\/usr/bin/setsid /usr/bin/ssh ", username, "@", hostname, "\n"]

echoPassContents :: FilePath -> Text -> Text
echoPassContents fname password = T.concat ["#!/bin/sh\n",
	"set -f\necho '", password, "'\nrm ", T.pack fname]

prepareSshPassword :: Text -> FilePath -> IO [(String, String)]
prepareSshPassword password tmpDir = do
	let echoPassPath = tmpDir </> "echopass.sh"
	writeTempScript echoPassPath (echoPassContents echoPassPath password)
	parentEnv <- filter ((/= "SSH_ASKPASS") . fst) <$> getEnvironment
	let sysEnv = [("SSH_ASKPASS", echoPassPath)]
	return $ parentEnv ++ sysEnv

-- I have to jump through several hoops to make this work...
-- I open in an external terminal so I can't communicate
-- with the ssh app in the terminal, but environment variables
-- get propagated. I must use a temporary file to store the
-- password, but it's quickly deleted and with 700 permissions.
openSshSession :: Text -> Text -> Text -> IO (Either Text ())
openSshSession server username password = do
	tmpDir <- getTemporaryDirectory
	-- TODO here I'm creating a temp file but not deleting it..
	-- it is small though and tmpfs will not persist across reboots.
	let runSshPath = tmpDir </> "runssh.sh"
	writeTempScript runSshPath (runSshContents server username)
	let params = ["-e", runSshPath]
	sshEnv <- prepareSshPassword password tmpDir
	-- TODO detect other xterm types than gnome-terminal
	fmapL textEx <$> (try $ void $
		createProcess (proc "gnome-terminal" params)
			{ env = Just sshEnv })

runProgramOverSshAsync :: Text -> Text -> Text -> Maybe Text -> Text
	-> (CommandProgress -> IO ()) -> IO ()
runProgramOverSshAsync server username password workDir program readCallback = do
	sshEnv <- getTemporaryDirectory >>= prepareSshPassword password
	let workDirCommand = maybe "" (\dir -> "cd " ++ dir ++ ";") (T.unpack <$> workDir)
	let command = workDirCommand ++ T.unpack program
	let params = ["/usr/bin/ssh", T.unpack username ++ "@" ++ T.unpack server, command]
	tryCommandAsync "setsid" params Nothing (Just sshEnv) readCallback
