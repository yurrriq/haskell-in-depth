{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Suntimes.Types where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Linear (V2 (..))

type Address = Text

newtype GeoCoords = GeoCoords (V2 Text)
  deriving (Generic, Show)

instance FromJSON GeoCoords where
  parseJSON (Object o) =
    GeoCoords <$> (V2 <$> o .: "lat" <*> o .: "lng")
  parseJSON invalid =
    prependFailure
      "parsing GeoCoords failed, "
      (typeMismatch "Object" invalid)

data SunTimes dt = SunTimes
  { sunrise :: dt,
    sunset :: dt
  }
  deriving (Show, Generic, FromJSON)

data WebAPIAuth = WebAPIAuth
  { timeZoneDBkey :: Text,
    email :: Text,
    agent :: Text
  }
  deriving (Show, Generic, FromJSON)

data When
  = Now
  | On Day
  deriving (Eq, Show)
