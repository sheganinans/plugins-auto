module Paths_plugins_auto (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,0,4] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/a2/Projects/tradeStation/Haskell/.cabal-sandbox/bin"
libdir     = "/home/a2/Projects/tradeStation/Haskell/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.1/plugi_3vMMbcXeHlTJ5AUI7PTU6r"
datadir    = "/home/a2/Projects/tradeStation/Haskell/.cabal-sandbox/share/x86_64-linux-ghc-7.10.1/plugins-auto-0.0.4"
libexecdir = "/home/a2/Projects/tradeStation/Haskell/.cabal-sandbox/libexec"
sysconfdir = "/home/a2/Projects/tradeStation/Haskell/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "plugins_auto_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "plugins_auto_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "plugins_auto_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "plugins_auto_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "plugins_auto_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
