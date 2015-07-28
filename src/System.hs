{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables, RecordWildCards #-}

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
import Network.Socket

import Util

sshDefaultPort :: Int
sshDefaultPort = 22

data ServerInfo = ServerInfo {
        srvAddress  :: Text,
        srvUsername :: Text,
        srvPassword :: Text
    }

runRdp :: ServerInfo -> Int -> Int -> IO (Either Text Text)
runRdp ServerInfo{..} width height = do
    homeDir <- T.pack <$> getHomeDirectory
    let params = T.unpack <$> [srvAddress,
                               "-u", srvUsername,
                               "-g", T.concat $ T.pack <$> [show width, "x", show height],
                               "-r", T.concat ["disk:mydisk=", homeDir],
                               "-p", "-"]
    r <- try (createProcess (proc "rdesktop" params) { std_in = CreatePipe})
    case r of
        Right (Just stdin, _, _, _) -> do
            hPutStr stdin (T.unpack srvPassword)
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
isHostTrusted :: Text -> IO (Either Text Bool)
isHostTrusted hostname = tryText $ do
    knownHostsFname <- getKnownHostsFilename
    let readLines = fmap (T.splitOn "\n") . T.hGetContents
    let linesReadHost = fmap (head . T.splitOn " ")
    hosts <- withFile knownHostsFname ReadMode (fmap linesReadHost . readLines)
    return $ hostname `elem` hosts

getHostKeyDetails :: Text -> IO Text
getHostKeyDetails hostname = T.pack <$>
    readProcess "ssh-keyscan" [T.unpack hostname] ""

addInHostTrustStore :: Text -> IO (Either Text ())
addInHostTrustStore server = tryText $ do
    knownHostsFname <- getKnownHostsFilename
    hostKeyDetails <- getHostKeyDetails server
    let addKeyDetails hndl = T.hPutStr hndl ("\n" <> hostKeyDetails)
    withFile knownHostsFname AppendMode addKeyDetails

runSshContents :: FilePath -> Text -> Text -> Int -> Text
runSshContents fname hostname username port = T.concat ["#!/usr/bin/sh\n\
    \rm ", T.pack fname, "\n\
    \/usr/bin/setsid /usr/bin/ssh ", username, "@", hostname, " -p ", text port, "\n"]

runSshContentsCommand :: FilePath -> Text -> Text -> Text -> Text
runSshContentsCommand fname hostname username command = T.concat ["#!/usr/bin/sh\n\
    \rm ", T.pack fname, "\n\
    \/usr/bin/setsid /usr/bin/ssh ", username, "@", hostname, " -t '", command, "; bash -l'"]

runSshContentsTunnel :: FilePath -> Text -> Text -> Int -> Text
runSshContentsTunnel fname hostname username port = T.concat ["#!/usr/bin/sh\n\
    \rm ", T.pack fname, "\n\
    \/usr/bin/setsid /usr/bin/ssh -L ", text port, ":localhost:", text port,
    " ", username, "@", hostname, "\n"]

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

data SshCommandOptions = JustSsh { sshoTerminal :: Bool, sshoPort :: Int }
                       | SshCommand Text
                       | SshTunnel Int

-- I have to jump through several hoops to make this work...
-- I open in an external terminal so I can't communicate
-- with the ssh app in the terminal, but environment variables
-- get propagated. I must use a temporary file to store the
-- password, but it's quickly deleted and with 700 permissions.
openSshSession :: ServerInfo -> SshCommandOptions -> IO (Either Text ())
openSshSession ServerInfo{..} sshCommandOptions = do
    tmpDir <- getTemporaryDirectory
    -- TODO here I'm creating a temp file but not deleting it..
    -- it is small though and tmpfs will not persist across reboots.
    let runSshPath = tmpDir </> "runssh.sh"
    let scriptContents = case sshCommandOptions of
         JustSsh _ port  -> runSshContents runSshPath srvAddress srvUsername port
         SshCommand cmd  -> runSshContentsCommand runSshPath srvAddress srvUsername cmd
         SshTunnel port  -> runSshContentsTunnel runSshPath srvAddress srvUsername port
    writeTempScript runSshPath scriptContents
    let openTerminal = case sshCommandOptions of
            JustSsh True _ -> True
            SshCommand _   -> True
            _              -> False
    -- TODO detect other xterm types than gnome-terminal
    let (cmd, params) = if openTerminal
                           then ("gnome-terminal", ["-e", runSshPath])
                           else (runSshPath, [])
    sshEnv <- prepareSshPassword srvPassword tmpDir
    tryText $ void $
        createProcess (proc cmd params)
            { env = Just sshEnv }

openSshTunnelTerminal :: Int -> ServerInfo -> ServerInfo -> IO (Either Text ())
openSshTunnelTerminal portTunnel intermediate final =
    openSshTunnelSession portTunnel intermediate final $ \port srv ->
        void $ openSshSession srv (JustSsh True port)

openSshTunnelSession :: Int -> ServerInfo -> ServerInfo
                     -> (Int -> ServerInfo -> IO ())
                     -> IO (Either Text ())
openSshTunnelSession portTunnel intermediate final callback = tryText $ do
    isTunnelOpen <- not <$> isPortFree portTunnel
    if (not isTunnelOpen)
       then do
          void $ openSshSession intermediate $ SshTunnel portTunnel
          -- the first tunnel from me to the intermediate
          -- host is now open. Now open the tunnel from the
          -- second host to the final host.
          -- ### TODO try to reuse runSshContentsTunnel
          let secondTunnelCmd = "ssh -t -t -L " <> text portTunnel <> ":localhost:22 " <> sshUserHost final
          let finalShellHandler = sshTunnelOpenShellHandler portTunnel final callback
          runProgramOverSshAsync intermediate Nothing secondTunnelCmd finalShellHandler
       else
          -- the tunnel is already open
          callback portTunnel (ServerInfo "127.0.0.1" (srvUsername final) (srvPassword final))

sshTunnelOpenShellHandler :: Int -> ServerInfo
                          -> (Int -> ServerInfo -> IO ()) -> CommandProgress
                          -> IO ()
sshTunnelOpenShellHandler port ServerInfo{..} callback = \case
    CommandOutput _   -> return ()
    CommandFailed msg -> putStrLn $ "*** failed establishing tunnel:" <> show msg -- should report errors better!
    CommandSucceeded  -> callback port srv
                         where
                           srv = ServerInfo "127.0.0.1" srvUsername srvPassword

sshHandlePasswordAndRun :: Text -> [Text] -> (CommandProgress -> IO ()) -> IO ()
sshHandlePasswordAndRun password sshCommandParams readCallback = do
    sshEnv <- getTemporaryDirectory >>= prepareSshPassword password
    tryCommandAsync "setsid" sshCommandParams Nothing (Just sshEnv) readCallback

sshUserHost :: ServerInfo -> Text
sshUserHost ServerInfo{..} = srvUsername <> "@" <> srvAddress

runProgramOverSshAsync :: ServerInfo -> Maybe Text -> Text
    -> (CommandProgress -> IO ()) -> IO ()
runProgramOverSshAsync srv@ServerInfo{..} workDir program readCallback = do
    let workDirCommand = maybe "" (\dir -> "cd " <> dir <> ";") workDir
    let command = workDirCommand <> program
    let params = ["/usr/bin/ssh", sshUserHost srv, command]
    sshHandlePasswordAndRun srvPassword params readCallback

downloadFileSsh :: ServerInfo -> Text -> (CommandProgress -> IO ()) -> IO ()
downloadFileSsh srv@ServerInfo{..} path readCallback = do
    outputDir <- getUserDir "DOWNLOAD"
    let fname = takeFileName (T.unpack path)
    readCallback $ CommandOutput $ T.pack $ printf "\nStarting download of the file %s to %s..." fname outputDir
    sshHandlePasswordAndRun srvPassword ["/usr/bin/scp",
        sshUserHost srv <> ":" <> path, T.pack outputDir] readCallback

-- alternative implementations: http://stackoverflow.com/a/28101291/516188
saveAuthKeyBytes ::Text -> Maybe BS.ByteString -> IO (Either Text Text)
saveAuthKeyBytes path bytes  = runExceptT $ do
    targetFile <- hoistEither $ note "Invalid target file name"
        $ T.stripPrefix "file://" path
    key <- hoistEither $ note "No authentication key for that server!" bytes
    bimapExceptT textEx (const "") . ExceptT . try
        $ BS.writeFile (T.unpack targetFile) key

isPortFree :: Int -> IO Bool
isPortFree port = do
    s <- socket AF_INET Stream defaultProtocol
    localhost <- inet_addr "127.0.0.1"
    portOpen <- try (connect s (SockAddrInet (fromIntegral port) localhost))
    case portOpen of
        Left (_ :: SomeException) -> return True
        Right () -> close s >> return False
