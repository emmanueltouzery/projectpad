import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup (ConfigFlags, InstallFlags, BuildFlags)
import Distribution.Simple.InstallDirs

import System.Directory
import Data.List
import Control.Applicative
import System.FilePath.Posix

main = defaultMainWithHooks simpleUserHooks
	{
		postBuild = doPostBuild
	}

getAppDataDir :: PackageDescription -> LocalBuildInfo -> IO FilePath
getAppDataDir pkg_descr lbi = do
	let idt = installDirTemplates lbi
	let env = installDirsTemplateEnv idt
	let idt' = fmap (fromPathTemplate
	      . substPathTemplate env
	      . substPathTemplate (packageTemplateEnv (package pkg_descr))) idt
	let appDataDir = datadir idt' ++ "/" ++ datasubdir idt' 
	createDirectoryIfMissing True appDataDir
	return appDataDir

-- i found it difficult to find information about this...
-- helped myself also with this code: https://github.com/Eelis/geordi/blob/master/Setup.hs
doPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
doPostBuild _ _ pkg_descr lbi = do
	appDataDir <- getAppDataDir pkg_descr lbi
	let getResourceFiles subfolder = map (subfolder </>)
		<$> filter (not . (isSuffixOf ".TXT")) -- just for the pics license text file
		<$> filter (not . (isPrefixOf "."))
		<$> getDirectoryContents subfolder
	let copyResources subfolder = do
		createDirectoryIfMissing True (appDataDir </> subfolder)
		sourceFiles <- getResourceFiles subfolder
		mapM_ (\s -> copyFile s (appDataDir </> s)) sourceFiles
	mapM_ copyResources ["qml", "glyphicons-free", "pics"]
