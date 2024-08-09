{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_lista02 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/guilherme/Documents/dev/haskell/lista02/.stack-work/install/x86_64-linux/1c9da0ed423ce3358d4278a65a432a64be23547091ef8b9205cfdd05f6680ffb/9.6.6/bin"
libdir     = "/home/guilherme/Documents/dev/haskell/lista02/.stack-work/install/x86_64-linux/1c9da0ed423ce3358d4278a65a432a64be23547091ef8b9205cfdd05f6680ffb/9.6.6/lib/x86_64-linux-ghc-9.6.6/lista02-0.1.0.0-2tuckcvaYMl3pqHYOuinwv-lista02"
dynlibdir  = "/home/guilherme/Documents/dev/haskell/lista02/.stack-work/install/x86_64-linux/1c9da0ed423ce3358d4278a65a432a64be23547091ef8b9205cfdd05f6680ffb/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/guilherme/Documents/dev/haskell/lista02/.stack-work/install/x86_64-linux/1c9da0ed423ce3358d4278a65a432a64be23547091ef8b9205cfdd05f6680ffb/9.6.6/share/x86_64-linux-ghc-9.6.6/lista02-0.1.0.0"
libexecdir = "/home/guilherme/Documents/dev/haskell/lista02/.stack-work/install/x86_64-linux/1c9da0ed423ce3358d4278a65a432a64be23547091ef8b9205cfdd05f6680ffb/9.6.6/libexec/x86_64-linux-ghc-9.6.6/lista02-0.1.0.0"
sysconfdir = "/home/guilherme/Documents/dev/haskell/lista02/.stack-work/install/x86_64-linux/1c9da0ed423ce3358d4278a65a432a64be23547091ef8b9205cfdd05f6680ffb/9.6.6/etc"

getBinDir     = catchIO (getEnv "lista02_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "lista02_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "lista02_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "lista02_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lista02_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lista02_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
