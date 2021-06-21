{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Expr.Eval where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (traverse_)
-- import Data.Char
import Data.List (uncons)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)

type Stack = [Integer]

type EvalM = ExceptT EvalError (State (Stack, Stack))

data EvalError
  = EmptyStack
  | ExtraElements
  | NotANumber Text
  | EmptyStash
  deriving (Eq, Show)

push :: Integer -> EvalM ()
push x = modify (first (x :))

pop :: EvalM Integer
pop =
  get >>= \case
    ([], _ys) -> throwError EmptyStack
    (x : xs, ys) -> put (xs, ys) >> pure x

ensureSingletonStack :: EvalM ()
ensureSingletonStack = gets ((/= 1) . length) ~~> throwError ExtraElements

fetchItem :: EvalM Integer
fetchItem =
  gets (uncons . snd) >>= \case
    Nothing ->
      throwError EmptyStash
    Just (x, xs) ->
      modify (second (const xs)) >> pure x

readNumber :: Text -> EvalM Integer
readNumber txt =
  case decimal txt of
    Right (n, rest) | T.null rest -> pure n
    _ -> throwError (NotANumber txt)

clac :: Text -> EvalM Integer
clac txt =
  do
    put ([], [])
    traverse_ step (T.words txt)
    ensureSingletonStack
    pop

step :: Text -> EvalM ()
step "+" = binop (+)
step "*" = binop (*)
step "-" = binop (-)
step t = readNumber t >>= push

binop :: (Integer -> Integer -> Integer) -> EvalM ()
binop op = flip op <$> pop <*> pop >>= push

infixl 4 ~~>

(~~>) :: Monad m => m Bool -> m () -> m ()
x ~~> y = flip when y =<< x
