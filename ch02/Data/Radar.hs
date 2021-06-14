{-# LANGUAGE DeriveAnyClass #-}

module Data.Radar
  ( module Data.Radar.Parsers,
    module Data.Radar.Types,
    rotate,
    rotateMany,
    rotateManySteps,
    orient,
    orientMany,
    orientFromFile,
  )
where

import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Radar.Parsers
import Data.Radar.Types
import HID.CyclicEnum

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = flip (rotate . mconcat)

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl (flip rotate)

orient :: Direction -> Direction -> Turn
orient from to =
  fromJust $
    find (\turn -> rotate turn from == to) $
      enumFrom minBound

orientMany :: [Direction] -> [Turn]
orientMany ds@(_ : _ : _) = zipWith orient ds (tail ds)
orientMany _ = []

orientFromFile :: FilePath -> IO ()
orientFromFile = undefined
