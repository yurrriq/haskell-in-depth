module DirTree where

import App
import Control.Lens
import Utils

dirTree :: MyApp (FilePath, Int) state ()
dirTree =
  do
    ok <- (&&) . isDirectory <$> currentPathStatus <*> notTooDeep
    when ok $
      do
        curPath <- view path
        curDepth <- view depth
        tell [(takeBaseName curPath, curDepth)]
        traverseDirectoryWith dirTree
