module Data.Radar.Parsers where

import Control.Applicative ((<|>))
import Data.Radar.Types (Direction (..), Turn (..))
import Text.Trifecta (Parser, string, token)

direction :: Parser Direction
direction =
  North <$ token (string (show North))
    <|> East <$ token (string (show East))
    <|> South <$ token (string (show South))
    <|> West <$ token (string (show West))

turn :: Parser Turn
turn =
  TNone <$ token (string (show TNone))
    <|> TLeft <$ token (string (show TLeft))
    <|> TRight <$ token (string (show TRight))
    <|> TAround <$ token (string (show TAround))
