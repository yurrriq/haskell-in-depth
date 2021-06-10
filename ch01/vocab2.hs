import Control.Arrow ((&&&))
import Data.Char (isLetter)
import Data.List (group, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Environment (getArgs)

type Entry = (Text, Int)

type Vocabulary = [Entry]

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [fname] -> processTextFile fname
      _ -> putStrLn "Usage: vocab-builder filename"

processTextFile :: FilePath -> IO ()
processTextFile fname =
  printAllWords =<< extractVocab <$> TextIO.readFile fname

extractVocab :: Text -> Vocabulary
extractVocab text = map buildEntry (group (sort wordz))
  where
    wordz =
      map Text.toCaseFold $
        filter (not . Text.null) $
          map cleanWord $
            Text.words text
    buildEntry = head &&& length
    cleanWord = Text.dropAround (not . isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocab =
  do
    putStrLn "All words: "
    TextIO.putStrLn (Text.unlines (map fst vocab))
