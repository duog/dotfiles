{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_doug_xmonad (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/doug/.cabal/bin"
libdir     = "/home/doug/.cabal/lib/x86_64-linux-ghc-8.4.3/doug-xmonad-0.1.0.0-8QIIHi5aKhFFgbBMmRkNy3-doug-xmonad"
dynlibdir  = "/home/doug/.cabal/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/doug/.cabal/share/x86_64-linux-ghc-8.4.3/doug-xmonad-0.1.0.0"
libexecdir = "/home/doug/.cabal/libexec/x86_64-linux-ghc-8.4.3/doug-xmonad-0.1.0.0"
sysconfdir = "/home/doug/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "doug_xmonad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "doug_xmonad_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "doug_xmonad_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "doug_xmonad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "doug_xmonad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "doug_xmonad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
