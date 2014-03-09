module Paths_Operads (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/dronte/.cabal/bin"
libdir     = "/home/dronte/.cabal/lib/Operads-1.1/ghc-7.6.3"
datadir    = "/home/dronte/.cabal/share/Operads-1.1"
libexecdir = "/home/dronte/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Operads_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Operads_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Operads_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Operads_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
