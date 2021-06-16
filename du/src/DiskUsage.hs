{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module DiskUsage where

import App
import Utils

data DUEntryAction
  = TraverseDir {dirpath :: FilePath, requireReporting :: Bool}
  | RecordFileSize {fsize :: FileOffset}
  | None

diskUsage :: MyApp (FilePath, FileOffset) FileOffset ()
diskUsage = processEntry =<< decide <$> ask <*> currentPathStatus
  where
    decide :: AppEnv -> FileStatus -> DUEntryAction
    decide appEnv@AppEnv {_path} fs
      | isDirectory fs = TraverseDir _path (notTooDeep' appEnv)
      | isRegularFile fs && checkExtension' appEnv = RecordFileSize (fileSize fs)
      | otherwise = None

    processEntry TraverseDir {..} =
      do
        usageOnEntry <- get
        traverseDirectoryWith diskUsage
        when requireReporting $
          do
            usageOnExit <- get
            tell [(dirpath, usageOnExit - usageOnEntry)]
    processEntry RecordFileSize {fsize} = modify (+ fsize)
    processEntry None = pure ()
