{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Expr.Eval where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (traverse_)
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

clac :: Text -> EvalM Integer
clac txt =
  do
    put ([], [])
    traverse_ step (T.words txt)
    ensureSingletonStack
    pop
  where
    step :: Text -> EvalM ()
    step "+" = binop (+)
    step "*" = binop (*)
    step "-" = binop (-)
    step "." = pop >>= stashItem
    step ":" = gets fst >>= stashStack >> modify (first (const []))
    step "," = fetchItem >>= push
    step ";" = fetchStack
    step t = readNumber t >>= push

    binop :: (Integer -> Integer -> Integer) -> EvalM ()
    binop op = flip op <$> pop <*> pop >>= push

push :: Integer -> EvalM ()
push = modify . first . (:)

pop :: EvalM Integer
pop =
  get >>= \case
    ([], _ys) -> throwError EmptyStack
    (x : xs, ys) -> put (xs, ys) >> pure x

ensureSingletonStack :: EvalM ()
ensureSingletonStack = gets ((/= 1) . length) ~~> throwError ExtraElements

stashItem :: Integer -> EvalM ()
stashItem = modify . second . (:)

stashStack :: Stack -> EvalM ()
stashStack = traverse_ (modify . second . (:))

fetchItem :: EvalM Integer
fetchItem =
  gets (uncons . snd) >>= \case
    Nothing ->
      throwError EmptyStash
    Just (x, xs) ->
      modify (second (const xs)) >> pure x

fetchStack :: EvalM ()
fetchStack = gets snd >>= traverse_ push >> modify (second (const []))

readNumber :: Text -> EvalM Integer
readNumber txt =
  case decimal txt of
    Right (n, rest) | T.null rest -> pure n
    _ -> throwError (NotANumber txt)

infixl 4 ~~>

(~~>) :: Monad m => m Bool -> m () -> m ()
x ~~> y = flip when y =<< x
