{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Suntimes where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Linear (V2 (..))
import Network.HTTP.Req
import Suntimes.App
import Suntimes.STExcept
import Suntimes.Types

newtype SunTimesWrapper dt = SunTimesWrapper {results :: SunTimes dt}
  deriving (Show, Generic, FromJSON)

getSunTimes :: GeoCoords -> When -> MyApp (SunTimes ZonedTime)
getSunTimes coords day =
  do
    SunTimes {..} <- getSunTimesUTC coords day `catch` noTimeHandler
    localTimeZone <- lookupTimeZone coords sunrise `catchAll` const (pure utc)
    pure $
      SunTimes
        (utcToZonedTime localTimeZone sunrise)
        (utcToZonedTime localTimeZone sunset)
  where
    noTimeHandler :: MonadThrow m => SunInfoException -> m a
    noTimeHandler (ServiceAPIError _) = throwM (UnknownTime coords)
    noTimeHandler ex = throwM ex

getSunTimesUTC :: GeoCoords -> When -> MyApp (SunTimes UTCTime)
getSunTimesUTC (GeoCoords (V2 lat lng)) cuando =
  handle rethrowReqException $
    liftIO $
      runReq defaultHttpConfig $
        do
          res <- req GET url NoReqBody jsonResponse params
          pure (results (responseBody res))
  where
    url = https "api.sunrise-sunset.org" /: "json"
    params =
      mconcat $
        [ "lat" =: lat,
          "lng" =: lng,
          "formatted" =: (0 :: Int)
        ]
          ++ whenToOptions cuando
    whenToOptions Now = []
    whenToOptions (On day) =
      ["date" =: formatTime defaultTimeLocale "%Y-%m-%d" day]

data TimeZoneInfo = TimeZoneInfo
  { gmtOffset :: Int,
    abbreviation :: String,
    dst :: String
  }
  deriving (Show, Generic, FromJSON)

lookupTimeZone :: GeoCoords -> UTCTime -> MyApp TimeZone
lookupTimeZone (GeoCoords (V2 lat lng)) time =
  do
    key <- asks timeZoneDBkey
    let url = http "api.timezondb.com" /: "v2.1" /: "get-time-zone"
        params =
          mconcat
            [ "key" =: key,
              "lat" =: lat,
              "lng" =: lng,
              "time" =: formatTime defaultTimeLocale "%s" time,
              "format" =: ("json" :: Text),
              "fields" =: ("gmtOffset,abbreviation,dst" :: Text),
              "by" =: ("position" :: Text)
            ]
    res <-
      liftIO $
        runReq defaultHttpConfig $
          req GET url NoReqBody jsonResponse params
    pure (tziToTimeZone (responseBody res))
  where
    tziToTimeZone TimeZoneInfo {..} =
      (secondsToTimeZone gmtOffset)
        { timeZoneName = abbreviation,
          timeZoneSummerOnly = dst == "1"
        }
    secondsToTimeZone seconds = minutesToTimeZone (seconds `div` 60)
