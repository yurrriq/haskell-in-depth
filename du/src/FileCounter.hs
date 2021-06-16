module FileCounter (fileCount) where

import App
import Control.Lens
import System.Directory.Extra (listFiles)
import Utils

fileCount :: MyApp (FilePath, Int) s ()
fileCount =
  do
    fs <- currentPathStatus
    guard (isDirectory fs)
    guard =<< notTooDeep
    curPath <- view path
    files <- liftIO (listFiles curPath)
    config <- view cfg
    tell [(curPath, length (filter (checkExtension config) files))]
