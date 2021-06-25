{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Suntimes.ProcessRequest where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Suntimes
import Suntimes.App
import Suntimes.GeoCoordsReq
import Suntimes.STExcept
import Suntimes.Types

processInteractively :: MyApp ()
processInteractively = action `catch` handler
  where
    action =
      do
        liftIO (TIO.putStrLn "> ")
        req <- liftIO TIO.getLine
        res <- processRequest req
        liftIO (TIO.putStrLn res)
    handler :: SunInfoException -> MyApp ()
    handler ex@(NetworkError _) = liftIO (print ex)
    handler ex =
      do
        liftIO $
          TIO.putStr $
            "There was an error while processing your request: "
              <> T.pack (show ex)
              <> "\nDo you want to try again (y/n)?"
        yesno <- liftIO TIO.getLine
        when (yesno `elem` ["y", "Y", "yes"]) processInteractively

processMany :: [Text] -> MyApp ()
processMany = mapM_ processRequestWrapper
  where
    processRequestWrapper txt =
      unless ("#" `T.isPrefixOf` txt) $
        (processRequest txt >>= liftIO . TIO.putStrLn)
          `catch` handler txt `finally` delaySec 3
    delaySec = liftIO . threadDelay . (1000000 *)
    handler :: Text -> SunInfoException -> MyApp ()
    handler txt ex =
      liftIO . TIO.putStrLn $
        "Error in request '" <> txt <> "': " <> T.pack (show ex)

processRequest :: Text -> MyApp Text
processRequest txt = go (parseRequestLine (T.strip txt))
  where
    go (Left ex) = throwM (FormatError ex)
    go (Right (addr, day)) =
      do
        coords <- getCoords addr
        sunTimes <- getSunTimes coords day
        pure (formatResult addr sunTimes defaultTimeLocale)

formatResult :: Text -> SunTimes ZonedTime -> TimeLocale -> Text
formatResult req SunTimes {..} locale =
  mconcat [day, " @ ", req, "\n    ", fmt sunrise, "\n    ", fmt sunset]
  where
    day = T.pack (formatTime locale "%x" sunrise)
    fmt = T.pack . formatTime locale "%X %Z"

parseRequestLine :: Text -> Either RequestError (Text, When)
parseRequestLine = parse . split
  where
    split :: Text -> (Text, Text)
    split txt =
      case T.breakOn "@" txt of
        (addr, "") -> ("", addr)
        (date, addr) -> (T.strip date, T.strip (T.tail addr))
    parse :: (Text, Text) -> Either RequestError (Text, When)
    parse (_, "") = Left EmptyRequest
    parse (date, addr) =
      case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack date) of
        Nothing -> Left (WrongDay date)
        Just day -> Right (addr, On day)
