import Data.Char (isLetter)
import Data.List (group, sort)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Environment (getArgs)

main :: IO ()
main =
  do
    [fname] <- getArgs
    text <- TextIO.readFile fname
    let wordz =
          map head $
            group $
              sort $
                map Text.toCaseFold $
                  filter (not . Text.null) $
                    map (Text.dropAround $ not . isLetter) $
                      Text.words text
    TextIO.putStrLn $ Text.unwords wordz
    print $ length wordz
