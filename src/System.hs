{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables, RecordWildCards, QuasiQuotes #-}

module System where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Text.Printf.TH
import Network.Socket
import Data.Maybe

import Util

sshDefaultPort :: Int
sshDefaultPort = 22

data ServerInfo = ServerInfo {
        srvAddress  :: Text,
        srvUsername :: Text,
        srvPassword :: Text
    } deriving Show

runRdp :: ServerInfo -> Int -> Int -> IO (Either Text Text)
runRdp ServerInfo{..} width height = do
    homeDir <- T.pack <$> getHomeDirectory
    let params = T.unpack <$> ["/v:" <> srvAddress,
                               "+clipboard",
                               "/cert-tofu", -- accept cert on first use,
                                             -- otherwise sometimes I get mismatch
                                             -- between IP & hostname & cert is rejected
                               "/u:" <> srvUsername,
                               "/w:" <> T.pack (show width),
                               "/h:" <> T.pack (show height),
                               "/drive:remote," <> homeDir,
                               "/from-stdin"]
    r <- try (createProcess (proc "xfreerdp" params)
             { std_in = CreatePipe, std_err = CreatePipe})
    case r of
        Right (Just stdin, _, Just hStderr, pHndl) -> do
            hPutStr stdin (T.unpack srvPassword)
            hFlush stdin
            t <- T.hGetChunk hStderr
            if T.null t || t == "Password: "
                then return (Right "")
                -- abort in case something else than 'Password: ' is printed on stderr
                -- happens when the certificate on the server has changed.
                else terminateProcess pHndl *> return (Left t)
        Left x -> return $ Left $ textEx x
        _ -> error "run RDP unexpected process output"

sysRemoveFromRdpTrustStore :: Text -> IO ()
sysRemoveFromRdpTrustStore addr = do
    homeDir <- getHomeDirectory
    let storePath = homeDir </> ".config/freerdp/known_hosts2"
    records <- T.lines <$> T.readFile storePath
    T.writeFile storePath $ T.unlines (filter (not . T.isPrefixOf addr) records)

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
        Left (SomeException x) -> do
            readCallback (CommandFailed $ T.pack $ show x)
            return (Left $ T.pack $ show x)
        _ -> error "Try command unexpected process output"

readHandleProgress :: Handle -> (CommandProgress -> IO ()) -> ProcessHandle -> IO ()
readHandleProgress hndl cmdProgress phndl = do
    chunk <- T.hGetChunk hndl
    if not $ T.null chunk
        then do
            cmdProgress $ CommandOutput chunk
            readHandleProgress hndl cmdProgress phndl
        else do
            mExitCode <- getProcessExitCode phndl
            case mExitCode of
                Nothing -> readHandleProgress hndl cmdProgress phndl
                Just exitCode -> handleProgramFinished cmdProgress exitCode

handleProgramFinished :: (CommandProgress -> IO ()) -> ExitCode -> IO ()
handleProgramFinished cmdProgress = \case
    ExitSuccess      -> cmdProgress CommandSucceeded
    ExitFailure code -> cmdProgress $
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
isHostTrusted :: Int -> ServerInfo -> IO (Either Text Bool)
isHostTrusted port ServerInfo{..} = tryText $ do
    let hostSignature = if port == sshDefaultPort
            then srvAddress
            else T.concat ["[", srvAddress, "]:", text port]
    knownHostsFname <- getKnownHostsFilename
    let readLines = fmap (T.splitOn "\n") . T.hGetContents
    let linesReadHost = fmap (head . T.splitOn " ")
    hosts <- withFile knownHostsFname ReadMode (fmap linesReadHost . readLines)
    return $ hostSignature `elem` hosts

getHostKeyDetails :: Text -> Int -> IO Text
getHostKeyDetails hostname port = T.pack <$>
    readProcess "ssh-keyscan" ["-p", show port, T.unpack hostname] ""

addInHostTrustStore :: Int -> ServerInfo -> IO (Either Text ())
addInHostTrustStore port ServerInfo{..} = tryText $ do
    knownHostsFname <- getKnownHostsFilename
    hostKeyDetails <- getHostKeyDetails srvAddress port
    let addKeyDetails hndl = T.hPutStr hndl ("\n" <> hostKeyDetails)
    withFile knownHostsFname AppendMode addKeyDetails

-- pretty sophisticated system, see http://unix.stackexchange.com/a/272524/36566
-- the basis is that we need setsid & SSH_ASKPASS to be able to save the
-- user from the need to enter the password.
-- The issue though is that setsid makes resizing the terminal work poorly.
-- let's say if you open a 'less' in the terminal it won't resize, and things
-- like that. So we take another approach. Use the SSH control master feature.
-- have SSH within setsid to establish the connection... Then we start a
-- second ssh without setsid, that one will reuse the existing connection.
runSshContents :: Text -> Text -> Int -> Maybe Text -> Text
runSshContents hostname username port mCmd = T.concat [
    "sh -c \"setsid ssh -o ControlPersist=30s -o ControlMaster=auto -nNS ", controlPath,
    " ", remoteDetails, ";",
    "ssh -S ", controlPath, cmdFlag, " ", remoteDetails, " ", fromMaybe "" mCmd, "\""]
    where
      cmdFlag = maybe "" (const " -t") mCmd
      controlPath = "~/.ssh/%C"
      remoteDetails = T.concat [username, "@", hostname, " -p ", text port]

runSshContentsTunnel :: Int -> ServerInfo -> ServerInfo -> Text
runSshContentsTunnel port intermediate final = T.concat [
    "/usr/bin/setsid -w /usr/bin/ssh ", sshUserHost intermediate,
    " -f -L ", text port, ":", srvAddress final, ":22 -N"]

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

data SshCommandOptions = JustSsh
                       | SshCommand Text
                       | SshTunnel ServerInfo
                       deriving Show

-- I have to jump through several hoops to make this work...
-- I open in an external terminal so I can't communicate
-- with the ssh app in the terminal, but environment variables
-- get propagated. I must use a temporary file to store the
-- password, but it's quickly deleted and with 700 permissions.
openSshSession :: ServerInfo -> Int -> SshCommandOptions -> IO (Either Text ())
openSshSession srv@ServerInfo{..} port sshCommandOptions = do
    let scriptContents = T.unpack $ case sshCommandOptions of
         JustSsh                -> runSshContents srvAddress srvUsername port Nothing
         SshCommand cmd         -> runSshContents srvAddress srvUsername port (Just cmd)
         SshTunnel intermediate -> runSshContentsTunnel port intermediate srv
    -- TODO detect other xterm types than gnome-terminal
    let (cmd, params) = case sshCommandOptions of
                          SshTunnel _ -> ("/usr/bin/sh", ["-c", scriptContents])
                          _           -> ("gnome-terminal", ["-e", scriptContents])
    sshEnv <- prepareSshPassword srvPassword =<< getTemporaryDirectory
    tryText $ void $
        createProcess (proc cmd params)
            { env = Just sshEnv }

openSshTunnelSession :: Int -> ServerInfo -> ServerInfo
                     -> (Int -> ServerInfo -> IO (Either Text a))
                     -> IO (Either Text a)
openSshTunnelSession portTunnel intermediate final callback = do
    isTunnelOpen <- not <$> isPortFree portTunnel
    unless isTunnelOpen $ do
        void $ openSshSession final portTunnel (SshTunnel intermediate)
        waitUntilPortIsOpen portTunnel
    -- the tunnel is now open
    callback portTunnel (ServerInfo "localhost" (srvUsername final) (srvPassword final))

waitUntilPortIsOpen :: Int -> IO ()
waitUntilPortIsOpen port = do
    isTunnelOpen <- not <$> isPortFree port
    unless isTunnelOpen $
        threadDelay 100000 >> waitUntilPortIsOpen port

isPortFree :: Int -> IO Bool
isPortFree port = do
    skt <- socket AF_INET Stream defaultProtocol
    localhost <- inet_addr "127.0.0.1"
    portOpen  <- try (connect skt (SockAddrInet (fromIntegral port) localhost))
    case portOpen of
        Left (_ :: SomeException) -> return True
        Right () -> close skt >> return False

sshHandlePasswordAndRun :: Text -> [Text] -> (CommandProgress -> IO ()) -> IO ()
sshHandlePasswordAndRun password sshCommandParams readCallback = do
    sshEnv <- getTemporaryDirectory >>= prepareSshPassword password
    tryCommandAsync "setsid" sshCommandParams Nothing (Just sshEnv) readCallback

sshUserHost :: ServerInfo -> Text
sshUserHost ServerInfo{..} = srvUsername <> "@" <> srvAddress

runProgramOverSshAsync :: ServerInfo -> Int -> Maybe Text -> Text
    -> (CommandProgress -> IO ()) -> IO ()
runProgramOverSshAsync srv@ServerInfo{..} port workDir program readCallback = do
    let workDirCommand = maybe "" (\dir -> "cd " <> dir <> ";") workDir
    let command = workDirCommand <> program
    let params = ["/usr/bin/ssh", sshUserHost srv, "-p", text port, command]
    sshHandlePasswordAndRun srvPassword params readCallback

downloadFileSsh :: ServerInfo -> Int -> Text -> (CommandProgress -> IO ()) -> IO ()
downloadFileSsh srv@ServerInfo{..} port path readCallback = do
    outputDir <- getUserDir "DOWNLOAD"
    let fname = takeFileName (T.unpack path)
    readCallback $ CommandOutput $ [st|\nStarting download of the file %s to %s...|] fname outputDir
    sshHandlePasswordAndRun srvPassword ["/usr/bin/scp", "-P", text port,
        sshUserHost srv <> ":" <> path, T.pack outputDir] readCallback

exportBytesToFile :: Text -> BS.ByteString -> IO Text
exportBytesToFile filename bytes = do
    outputDir <- getUserDir "DOCUMENTS"
    let (name, ext) = splitExtension (T.unpack filename)
    let getFullPath n = outputDir </> n <> ext
    let candidates = getFullPath <$> generateNames name
    firstFreeFname <- fromJust <$> findM (fmap not . doesFileExist) candidates
    BS.writeFile firstFreeFname bytes
    openInFileBrowser firstFreeFname
    return (T.pack firstFreeFname)

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []     = return Nothing
findM f (x:xs) = f x >>= bool (findM f xs) (return $ Just x)

generateNames :: String -> [String]
generateNames base = base : gen (1::Int) base
    where gen n name = [s|%s (%d)|] name n : gen (n+1) name

openInFileBrowser :: FilePath -> IO ()
openInFileBrowser fname = void $ createProcess (proc "nautilus" [fname])
