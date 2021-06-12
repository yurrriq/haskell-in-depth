import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Radar
import Data.Radar.Parsers
import Data.Radar.Types
import Paths_haskell_in_depth (getDataFileName)
import Text.Trifecta (Parser, Result (..), parseFromFileEx, some)

main :: IO ()
main =
  rotateFromFile North "data/turns.txt"

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fname =
  do
    turns <- parseInput (some turn) fname
    putStr "All turns: "
    putStrLn (intercalate " " (map show turns))
    let finalDir = rotateMany dir turns
    putStr "Final direction: "
    print finalDir
    let dirs = rotateManySteps dir turns
    putStr "Intermediate directions: "
    putStrLn (intercalate " " (map show dirs))

parseInput :: Parser a -> FilePath -> IO a
parseInput parser fname =
  do
    fname' <- getDataFileName fname
    parsed <- liftIO $ parseFromFileEx parser fname'
    case parsed of
      Success result -> pure result
      Failure reason -> error (show reason)
