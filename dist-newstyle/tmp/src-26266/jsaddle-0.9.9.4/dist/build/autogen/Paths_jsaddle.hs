{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_jsaddle (
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
version = Version [0,9,9,4] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/bass/.cabal/store/ghc-9.8.4-6626/jsaddle-0.9.9.4-37e24880e20780f9db70fb21cdc494239cd8076023c721414439e4001d166382/bin"
libdir     = "/home/bass/.cabal/store/ghc-9.8.4-6626/jsaddle-0.9.9.4-37e24880e20780f9db70fb21cdc494239cd8076023c721414439e4001d166382/lib"
dynlibdir  = "/home/bass/.cabal/store/ghc-9.8.4-6626/jsaddle-0.9.9.4-37e24880e20780f9db70fb21cdc494239cd8076023c721414439e4001d166382/lib"
datadir    = "/home/bass/.cabal/store/ghc-9.8.4-6626/jsaddle-0.9.9.4-37e24880e20780f9db70fb21cdc494239cd8076023c721414439e4001d166382/share"
libexecdir = "/home/bass/.cabal/store/ghc-9.8.4-6626/jsaddle-0.9.9.4-37e24880e20780f9db70fb21cdc494239cd8076023c721414439e4001d166382/libexec"
sysconfdir = "/home/bass/.cabal/store/ghc-9.8.4-6626/jsaddle-0.9.9.4-37e24880e20780f9db70fb21cdc494239cd8076023c721414439e4001d166382/etc"

getBinDir     = catchIO (getEnv "jsaddle_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "jsaddle_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "jsaddle_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "jsaddle_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "jsaddle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "jsaddle_sysconfdir") (\_ -> return sysconfdir)



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
