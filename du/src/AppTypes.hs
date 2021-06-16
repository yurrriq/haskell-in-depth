{-# LANGUAGE TemplateHaskell #-}

module AppTypes where

import Control.Lens (makeLenses, (^.))
import System.PosixCompat.Files
  ( FileStatus,
    getFileStatus,
    getSymbolicLinkStatus,
  )

data AppConfig = AppConfig
  { _basePath :: FilePath,
    _maxDepth :: Int,
    _extension :: Maybe String,
    _followSymlinks :: Bool
  }

makeLenses ''AppConfig

data AppEnv = AppEnv
  { _cfg :: AppConfig,
    _path :: FilePath,
    _depth :: Int,
    _fileStatus :: FilePath -> IO FileStatus
  }

makeLenses ''AppEnv

initialEnv :: AppConfig -> AppEnv
initialEnv config =
  AppEnv
    { _cfg = config,
      _path = config ^. basePath,
      _depth = 0,
      _fileStatus =
        if config ^. followSymlinks
          then getFileStatus
          else getSymbolicLinkStatus
    }
