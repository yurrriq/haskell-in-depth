{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Suntimes.STExcept where

import Control.Monad.Catch
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Client as Client
import Network.HTTP.Req
import Suntimes.Types

data RequestError
  = EmptyRequest
  | WrongDay Text
  deriving (Show)

data SunInfoException
  = UnknownLocation Text
  | UnknownTime GeoCoords
  | FormatError RequestError
  | ServiceAPIError Text
  | NetworkError SomeException
  | ConfigError
  deriving (Exception)

instance Show SunInfoException where
  show (UnknownLocation _) = "Failed to determine coordinates"
  show (UnknownTime _) = "Failed to determine sun{rise,set} times"
  show (FormatError err) = show err
  show (ServiceAPIError _) = "Error communicating with exeternal services"
  show (NetworkError _) = "Network communication error"
  show ConfigError = "Error parsing configuration file"

rethrowReqException :: MonadThrow m => HttpException -> m a
rethrowReqException (JsonHttpException s) = throwM (ServiceAPIError (T.pack s))
rethrowReqException
  ( VanillaHttpException
      ( Client.HttpExceptionRequest
          _
          (Client.StatusCodeException res _)
        )
    ) =
    throwM (ServiceAPIError (T.pack (show (Client.responseStatus res))))
rethrowReqException (VanillaHttpException e) = throwM (NetworkError (toException e))
