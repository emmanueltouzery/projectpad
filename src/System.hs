{-# LANGUAGE OverloadedStrings #-}

module System where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative
import System.Directory
import Control.Exception
import System.Process
import System.Exit
import GHC.IO.Handle
import System.IO hiding (stdin, stdout)
import System.Environment
import System.Posix.Files
import System.FilePath.Posix
import Control.Error
import Control.Monad
import Control.Concurrent
import Data.Monoid
import qualified Data.ByteString as BS
import System.Environment.XDG.UserDir
import Text.Printf

import Util

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

tryCommand :: Text -> [Text] -> Maybe FilePath
    -> Maybe [(String, String)] -> (CommandProgress -> IO ()) -> IO (Either Text ())
tryCommand cmd_ params_ mCwd envVal readCallback = do
    let cmd = T.unpack cmd_
    let params = T.unpack <$> params_
    r <- try (createProcess (proc cmd params)
        {
            std_out = CreatePipe,
            env = envVal,
            cwd = mCwd
        })
    case r of
        Right (_, Just stdout, _, phndl) -> Right <$> readHandleProgress stdout readCallback phndl
        Left (SomeException x) -> return $ Left $ T.pack $ show x
        _ -> error "Try command unexpected process output"

readHandleProgress :: Handle -> (CommandProgress -> IO ()) -> ProcessHandle -> IO ()
readHandleProgress hndl cmdProgress phndl = do
    chunk <- T.hGetChunk hndl
    cmdProgress $ CommandOutput chunk
    if not $ T.null chunk
        then readHandleProgress hndl cmdProgress phndl
        else handleProgramFinished cmdProgress phndl

handleProgramFinished :: (CommandProgress -> IO ()) -> ProcessHandle -> IO ()
handleProgramFinished cmdProgress phndl = do
    mExitCode <- getProcessExitCode phndl
    case mExitCode of
        Nothing -> cmdProgress $ CommandFailed "Can't get process exit code from handle"
        Just ExitSuccess -> cmdProgress CommandSucceeded
        Just (ExitFailure code) -> cmdProgress $
            CommandFailed (T.pack $ "Error code: " ++ show code)

tryCommandAsync :: Text -> [Text] -> Maybe FilePath
    -> Maybe [(String, String)] -> (CommandProgress -> IO ())  -> IO ()
tryCommandAsync cmd params mCwd envVal readCallback = do
    let runCmd = tryCommand cmd params mCwd envVal readCallback
    void $ forkFinally runCmd notifyIfFail
    where
        -- in case runCmd returns Right, it managed to start the external
        -- process, and we already notified of the success when it finished.
        -- If it returns Left however, we couldn't start it => tell about
        -- the failure.
        notifyIfFail (Left x) = readCallback $ CommandFailed $ textEx x
        notifyIfFail _ = return ()

openAssociatedFile :: Text -> IO (Either Text ())
openAssociatedFile path = do
    desktop <- getEnv "DESKTOP_SESSION"
    let openCmd = case desktop of
          "gnome" -> "gnome-open"
          _ -> "xdg-open"
    tryCommand openCmd [path] Nothing Nothing (const $ return ())

writeTempScript :: String -> Text -> IO ()
writeTempScript fname contents = do
    T.writeFile fname contents
    setFileMode fname ownerModes

getKnownHostsFilename :: IO String
getKnownHostsFilename = do
    homeDir <- getHomeDirectory
    return $ homeDir </> ".ssh/known_hosts"

-- It's a bit of an embarrasment. I launch ssh with setsid,
-- so that it doesn't ask for the password through STDIN but
-- rather uses the SSH_ASKPASS mechanism. But then if the
-- server fingerprint is not known it won't ask to accept it,
-- since it's not in interactive mode... So I must check myself
-- beforehand.
isHostTrusted :: Text -> IO Bool
isHostTrusted hostname = do
    knownHostsFname <- getKnownHostsFilename
    let readLines = fmap (T.splitOn "\n") . T.hGetContents
    let linesReadHost = fmap (head . T.splitOn " ")
    hosts <- withFile knownHostsFname ReadMode (fmap linesReadHost . readLines)
    return $ hostname `elem` hosts

getHostKeyDetails :: Text -> IO Text
getHostKeyDetails hostname = T.pack <$> readProcess "ssh-keyscan" [T.unpack hostname] ""

addInHostTrustStore :: Text -> IO (Either Text ())
addInHostTrustStore server = fmapL textEx <$> try (do
    knownHostsFname <- getKnownHostsFilename
    hostKeyDetails <- getHostKeyDetails server
    let addKeyDetails hndl = T.hPutStr hndl ("\n" <> hostKeyDetails)
    withFile knownHostsFname AppendMode addKeyDetails)

runSshContents :: FilePath -> Text -> Text -> Text
runSshContents fname hostname username = T.concat ["#!/usr/bin/sh\n\
    \rm ", T.pack fname, "\n\
    \/usr/bin/setsid /usr/bin/ssh ", username, "@", hostname, "\n"]

runSshContentsCommand :: FilePath -> Text -> Text -> Text -> Text
runSshContentsCommand fname hostname username command = T.concat ["#!/usr/bin/sh\n\
    \rm ", T.pack fname, "\n\
    \/usr/bin/setsid /usr/bin/ssh ", username, "@", hostname, " -t '", command, "; bash -l'"]

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
openSshSession :: Text -> Text -> Text -> Maybe Text -> IO (Either Text ())
openSshSession server username password command = do
    tmpDir <- getTemporaryDirectory
    -- TODO here I'm creating a temp file but not deleting it..
    -- it is small though and tmpfs will not persist across reboots.
    let runSshPath = tmpDir </> "runssh.sh"
    let scriptContents = case command of
          Nothing -> runSshContents runSshPath server username
          Just cmd -> runSshContentsCommand runSshPath server username cmd
    writeTempScript runSshPath scriptContents
    let params = ["-e", runSshPath]
    sshEnv <- prepareSshPassword password tmpDir
    -- TODO detect other xterm types than gnome-terminal
    fmapL textEx <$> try (void $
        createProcess (proc "gnome-terminal" params)
            { env = Just sshEnv })

sshHandlePasswordAndRun :: Text -> [Text] -> (CommandProgress -> IO ()) -> IO ()
sshHandlePasswordAndRun password sshCommandParams readCallback = do
    sshEnv <- getTemporaryDirectory >>= prepareSshPassword password
    tryCommandAsync "setsid" sshCommandParams Nothing (Just sshEnv) readCallback

runProgramOverSshAsync :: Text -> Text -> Text -> Maybe Text -> Text
    -> (CommandProgress -> IO ()) -> IO ()
runProgramOverSshAsync server username password workDir program readCallback = do
    let workDirCommand = maybe "" (\dir -> "cd " <> dir <> ";") workDir
    let command = workDirCommand <> program
    let params = ["/usr/bin/ssh", username <> "@" <> server, command]
    sshHandlePasswordAndRun password params readCallback

downloadFileSsh :: Text -> Text -> Text -> Text -> (CommandProgress -> IO ()) -> IO ()
downloadFileSsh server username password path readCallback = do
    outputDir <- getUserDir "DOWNLOAD"
    let fname = takeFileName (T.unpack path)
    readCallback $ CommandOutput $ T.pack $ printf "\nStarting download of the file %s to %s..." fname outputDir
    sshHandlePasswordAndRun password ["/usr/bin/scp", username <> "@" <> server <> ":" <> path, T.pack outputDir] readCallback

-- alternative implementations: http://stackoverflow.com/a/28101291/516188
saveAuthKeyBytes ::Text -> Maybe BS.ByteString -> IO (Either Text Text)
saveAuthKeyBytes path bytes  = runEitherT $ do
    targetFile <- hoistEither $ note "Invalid target file name"
        $ T.stripPrefix "file://" path
    key <- hoistEither $ note "No authentication key for that server!" bytes
    bimapEitherT textEx (const "") . EitherT . try
        $ BS.writeFile (T.unpack targetFile) key
