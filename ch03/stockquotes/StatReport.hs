module StatReport where

import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Time (diffDays)
import QuoteData

decimalPlacesFloating :: Int
decimalPlacesFloating = 2

data StatValue = StatValue
  { decimalPlaces :: Int,
    value :: Double
  }

data StatEntry = StatEntry
  { -- FIXME: qfield :: QField,
    meanVal :: StatValue,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int
  }

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

computeMinMaxDays :: (Ord a, Foldable t) => (QuoteData -> a) -> t QuoteData -> (a, a, Int)
computeMinMaxDays get quotes = (get minQ, get maxQ, days)
  where
    cmp = comparing get
    minQ = minimumBy cmp quotes
    maxQ = maximumBy cmp quotes
    days = fromIntegral $ abs $ diffDays (day minQ) (day maxQ)

statInfo :: (Functor t, Foldable t) => t QuoteData -> [StatEntry]
statInfo quotes = undefined
