{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Arrow ((&&&), (***), (>>>))
import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Data.Char (isLetter)
import Data.List (group, sort, sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Formatting (Format (..), bprint, int, later, sformat, stext, (%))
import Formatting.Combinators (indentedLines)
import Options.Applicative

type Entry = (Text, Int)

type Vocabulary = [Entry]

data VocabBuilder = VocabBuilder
  { _withAllWords :: Bool,
    _numberOfFrequentWords :: Int,
    _filePath :: FilePath
  }

makeLenses ''VocabBuilder

main :: IO ()
main = buildVocab =<< execParser opts
  where
    opts =
      info
        (vocabBuilder <**> helper)
        ( fullDesc
            <> progDesc "Extract the vocabulary of a text file and print reports about it"
            <> header "vocab-builder - extract vocabulary from a file"
        )

vocabBuilder :: Parser VocabBuilder
vocabBuilder =
  VocabBuilder
    <$> switch
      ( short 'a'
          <> help "Whether to print all words used in the text"
      )
    <*> option
      auto
      ( short 'n'
          <> help "The number of most frequently used words to print"
          <> showDefault
          <> value 5
          <> metavar "FREQ_WORDS_NUM"
      )
    <*> argument str (metavar "FILE")

buildVocab :: VocabBuilder -> IO ()
buildVocab opts =
  do
    vocab <- extractVocab <$> TIO.readFile (opts ^. filePath)
    when (opts ^. withAllWords) $
      TIO.putStrLn (allWordsReport vocab)
    TIO.putStrLn (wordCountReport vocab)
    TIO.putStrLn (frequentWordsReport vocab (opts ^. numberOfFrequentWords))

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
