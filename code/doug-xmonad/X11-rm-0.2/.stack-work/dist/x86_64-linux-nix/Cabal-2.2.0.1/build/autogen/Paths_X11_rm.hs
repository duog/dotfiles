{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_X11_rm (
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
version = Version [0,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/doug/code/doug-xmonad/.stack-work/install/x86_64-linux-nix/ghc-8.4.3/8.4.3/bin"
libdir     = "/home/doug/code/doug-xmonad/.stack-work/install/x86_64-linux-nix/ghc-8.4.3/8.4.3/lib/x86_64-linux-ghc-8.4.3/X11-rm-0.2-AtBZpFixmgmC5xUMMDeoBg"
dynlibdir  = "/home/doug/code/doug-xmonad/.stack-work/install/x86_64-linux-nix/ghc-8.4.3/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/doug/code/doug-xmonad/.stack-work/install/x86_64-linux-nix/ghc-8.4.3/8.4.3/share/x86_64-linux-ghc-8.4.3/X11-rm-0.2"
libexecdir = "/home/doug/code/doug-xmonad/.stack-work/install/x86_64-linux-nix/ghc-8.4.3/8.4.3/libexec/x86_64-linux-ghc-8.4.3/X11-rm-0.2"
sysconfdir = "/home/doug/code/doug-xmonad/.stack-work/install/x86_64-linux-nix/ghc-8.4.3/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "X11_rm_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "X11_rm_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "X11_rm_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "X11_rm_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "X11_rm_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "X11_rm_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
