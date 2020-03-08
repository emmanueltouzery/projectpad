import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup (ConfigFlags, InstallFlags, BuildFlags)

import System.Process
import System.Directory
import Data.List
import Control.Applicative
import System.FilePath.Posix

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    {
        postBuild = doPostBuild
    }

doPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
doPostBuild _ _ pkg_descr lbi = do
    let appDataDir = datadir $ absoluteInstallDirs pkg_descr lbi NoCopyDest
    let getResourceFiles subfolder =
            map (subfolder </>)
                <$> filter (not . isSuffixOf ".TXT") -- just for the pics license text file
                <$> filter (not . isPrefixOf ".")
                <$> filter (`notElem` ["core", "buttonstyles", "tiles"])
                <$> getDirectoryContents subfolder
    let copyResources subfolder = do
        createDirectoryIfMissing True (appDataDir </> subfolder)
        sourceFiles <- getResourceFiles subfolder
        mapM_ (\s -> copyFile s (appDataDir </> s)) sourceFiles
    mapM_ copyResources ["qml", "qml/core", "qml/buttonstyles", "qml/tiles",
                   "glyphicons-free", "pics", "help"]
    copyFile "projectpad-128.png" (appDataDir </> "projectpad-128.png") -- for the help HTML
    return ()
