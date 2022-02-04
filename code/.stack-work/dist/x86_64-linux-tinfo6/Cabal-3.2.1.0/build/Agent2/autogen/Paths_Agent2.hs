{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Agent2 (
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

bindir     = "/media/laura/Nuevo vol/Cybernetics/4to/2do semestre/Simulacion/proyecto agentes/RobotCasa/RobotHome/.stack-work/install/x86_64-linux-tinfo6/940ce2eb647a8801952a932fa6313769b7d90c7ad8e4ddc063627364b23041b0/8.10.7/bin"
libdir     = "/media/laura/Nuevo vol/Cybernetics/4to/2do semestre/Simulacion/proyecto agentes/RobotCasa/RobotHome/.stack-work/install/x86_64-linux-tinfo6/940ce2eb647a8801952a932fa6313769b7d90c7ad8e4ddc063627364b23041b0/8.10.7/lib/x86_64-linux-ghc-8.10.7/Agent2-0.1.0.0-4GFa7h4lhjQI3LAckpLSPo-Agent2"
dynlibdir  = "/media/laura/Nuevo vol/Cybernetics/4to/2do semestre/Simulacion/proyecto agentes/RobotCasa/RobotHome/.stack-work/install/x86_64-linux-tinfo6/940ce2eb647a8801952a932fa6313769b7d90c7ad8e4ddc063627364b23041b0/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/media/laura/Nuevo vol/Cybernetics/4to/2do semestre/Simulacion/proyecto agentes/RobotCasa/RobotHome/.stack-work/install/x86_64-linux-tinfo6/940ce2eb647a8801952a932fa6313769b7d90c7ad8e4ddc063627364b23041b0/8.10.7/share/x86_64-linux-ghc-8.10.7/Agent2-0.1.0.0"
libexecdir = "/media/laura/Nuevo vol/Cybernetics/4to/2do semestre/Simulacion/proyecto agentes/RobotCasa/RobotHome/.stack-work/install/x86_64-linux-tinfo6/940ce2eb647a8801952a932fa6313769b7d90c7ad8e4ddc063627364b23041b0/8.10.7/libexec/x86_64-linux-ghc-8.10.7/Agent2-0.1.0.0"
sysconfdir = "/media/laura/Nuevo vol/Cybernetics/4to/2do semestre/Simulacion/proyecto agentes/RobotCasa/RobotHome/.stack-work/install/x86_64-linux-tinfo6/940ce2eb647a8801952a932fa6313769b7d90c7ad8e4ddc063627364b23041b0/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Agent2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Agent2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Agent2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Agent2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Agent2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Agent2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
