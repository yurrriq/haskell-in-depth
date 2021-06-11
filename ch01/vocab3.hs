{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((&&&), (***), (>>>))
import Control.Monad (when)
import Data.Char (isLetter)
import Data.List (group, sort, sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Formatting (Format (..), bprint, int, later, sformat, stext, (%))
import Formatting.Combinators (indentedLines)
import System.Environment (getArgs)

type Entry = (Text, Int)

type Vocabulary = [Entry]

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      ["-a", fname, num] ->
        processTextFile fname True (read num)
      [fname, num] ->
        processTextFile fname False (read num)
      _ ->
        putStrLn "Usage: vocab-builder [-a] FILENAME FREQ_WORDS_NUM"

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile fname withAllWords n =
  do
    vocab <- extractVocab <$> TIO.readFile fname
    when withAllWords $
      TIO.putStrLn (allWordsReport vocab)
    TIO.putStrLn (wordCountReport vocab)
    TIO.putStrLn (frequentWordsReport vocab n)

allWordsReport :: Vocabulary -> Text
allWordsReport = T.unlines . ("All words" :) . allWords

allWords :: Vocabulary -> [Text]
allWords = map fst

extractVocab :: Text -> Vocabulary
extractVocab text = map buildEntry (group (sort wordz))
  where
    wordz =
      map T.toCaseFold $
        filter (not . T.null) $
          map cleanWord $
            T.words text
    buildEntry = head &&& length
    cleanWord = T.dropAround (not . isLetter)

wordCountReport :: Vocabulary -> Text
wordCountReport =
  wordCount
    >>> (mkReport "Total number of words" *** mkReport "Number of unique words")
    >>> uncurry T.append
  where
    mkReport = sformat (stext % ": " % int % "\n")

wordCount :: Vocabulary -> (Int, Int)
wordCount = (sum . map snd &&& length)

frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport vocab n =
  "Frequent words:\n"
    <> sformat (indentedLines 4 sentry) (take n (wordsByFrequency vocab))

sentry :: Format r (Entry -> r)
sentry = later (bprint stext . fst <> pure ": " <> bprint int . snd)

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing (Down . snd))
