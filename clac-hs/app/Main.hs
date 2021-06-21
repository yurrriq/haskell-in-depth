{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clac
import Control.Monad.Except
import Control.Monad.State
import Data.String (fromString)
import System.Console.Haskeline

main :: IO ()
main = runInputT settings loop
  where
    loop =
      do
        getInputLine "> " >>= \case
          Nothing -> pure ()
          Just input -> liftIO (process input) >> loop
    settings =
      Settings
        { complete = noCompletion,
          historyFile = Nothing,
          autoAddHistory = False
        }

process :: String -> IO ()
process line =
  do
    let (res, st) = runState (runExceptT (clac (fromString line))) ([], [])
    print st
    case res of
      Left (NotANumber _) ->
        putStrLn "= nan"
      Left _ ->
        pure ()
      Right ex ->
        putStr "= " >> print ex
