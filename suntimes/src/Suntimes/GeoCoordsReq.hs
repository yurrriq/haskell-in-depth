{-# LANGUAGE OverloadedStrings #-}

module Suntimes.GeoCoordsReq where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req
import Suntimes.App
import Suntimes.STExcept
import Suntimes.Types

getCoords :: Address -> MyApp GeoCoords
getCoords addr = handle rethrowReqException $
  do
    wauth <- ask
    let url = https "nominatim.openstreetmap.org" /: "search"
        params =
          mconcat
            [ "q" =: addr,
              "format" =: ("json" :: Text),
              "limit" =: (1 :: Int),
              "email" =: email wauth,
              header "User-Agent" (encodeUtf8 (agent wauth))
            ]
        request = req GET url NoReqBody jsonResponse params
    res <- liftIO $ responseBody <$> runReq defaultHttpConfig request
    case res of
      [] -> throwM (UnknownLocation addr)
      (coords : _) -> pure coords
