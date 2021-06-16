module Utils where

import App
import Control.Lens
import Data.Foldable (traverse_)
import System.Directory

traverseDirectoryWith :: MyApp logEntry state () -> MyApp logEntry state ()
traverseDirectoryWith app = view path >>= liftIO . listDirectory >>= traverse_ go
  where
    go name =
      flip local app $
        execState $
          do
            path %= \x -> x </> name
            depth += 1

currentPathStatus :: MyApp log state FileStatus
currentPathStatus = liftIO =<< view fileStatus <*> view path

checkExtension :: AppConfig -> FilePath -> Bool
checkExtension = flip (maybe True . flip isExtensionOf) . view extension

checkExtension' :: AppEnv -> Bool
checkExtension' = checkExtension <$> view cfg <*> view path

notTooDeep :: MyApp logEntry state Bool
notTooDeep = (<=) <$> view depth <*> (view maxDepth <$> view cfg)

notTooDeep' :: AppEnv -> Bool
notTooDeep' = (<=) <$> view depth <*> (view maxDepth <$> view cfg)
